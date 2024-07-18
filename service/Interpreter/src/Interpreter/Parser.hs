{-# LANGUAGE LambdaCase #-}

module Interpreter.Parser where

import Control.Applicative (Alternative (..), liftA2)
import Data.Bifunctor (first)
import Interpreter.Token (Token (..))
import Data.List (foldl')

data Expr
  = LitExpr Literal
  | IdentifierExpr ValName
  | LambdaExpr [ValName] Expr     -- Contracted lambda expression
  | SingleLambdaExpr ValName Expr -- Uncontracted lambda expression
  | ApplyExpr Expr [Expr]
  | SingleApplyExpr Expr Expr
  | LetExpr ValName Expr
  | LetInExpr [(ValName, Expr)] Expr
  | IfExpr Expr Expr Expr
  | BinOpExpr BinOp Expr Expr
  | ListExpr [Expr]
  deriving (Show, Eq)

type ValName = String

data Literal 
  = IntLitExpr Int
  | FloatLitExpr Float
  | BoolLitExpr Bool
  | StringLitExpr String
  | NilLitExpr
  deriving (Show, Eq, Ord)

data BinOp
  = LComposeExpr
  | RComposeExpr
  | LPipeExpr
  | RPipeExpr
  deriving (Show, Eq)

data Definition 
  = ValueDefinition ValueDefinition
  | ExprDefinition Expr
  deriving (Show, Eq)

data ValueDefinition
  = NameDefinition ValName Expr
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: [Token] -> [(a, [Token])]}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser $ \ts -> [(a, ts)]
  Parser f <*> Parser a = Parser $ \input -> do
    (f', rest) <- f input
    (a', s) <- a rest
    pure (f' a', s)

instance Alternative Parser where
  empty = Parser (const [])
  Parser a <|> Parser b =
    Parser $ \input -> a input ++ b input

data ParseError 
  = FailedParser
  | AmbiguousParse [(AST, [Token])] 
  deriving (Show)

data AST = AST [Definition] deriving (Show, Eq)

braced :: Parser a -> Parser [a]
braced p =
  token LBrace *> sepBy1 p (token $ Semicolon) <* token RBrace

satisfies :: (Token -> Bool) -> Parser Token
satisfies p =
  Parser $ \case
    (t: ts) | p t -> [(t, ts)]
    _ -> []

token :: Token -> Parser Token
token t = satisfies (== t)

pluck :: (Token -> Maybe a) -> Parser a
pluck f =
  Parser $ \case
    (t: ts) -> case f t of
      Just res -> [(res, ts)]
      Nothing -> []
    _ -> []

name :: Parser ValName
name =
  pluck $ \case
    Identifier s -> Just s
    _ -> Nothing

literal :: Parser Literal
literal = intLit <|> floatLit <|> stringLit <|> boolLit <|> nilLit
  where
    intLit :: Parser Literal
    intLit = pluck $ \case 
      IntLit i -> Just (IntLitExpr i)
      _ -> Nothing

    floatLit :: Parser Literal
    floatLit = pluck $ \case
      FloatLit f -> Just (FloatLitExpr f)
      _ -> Nothing

    stringLit :: Parser Literal
    stringLit = pluck $ \case
      StringLit s -> Just (StringLitExpr s)
      _ -> Nothing

    boolLit :: Parser Literal
    boolLit = pluck $ \case
      BoolLit b -> Just (BoolLitExpr b)
      _ -> Nothing

    nilLit :: Parser Literal
    nilLit = pluck $ \case
      NilLit -> Just NilLitExpr
      _ -> Nothing

definition :: Parser Definition
definition = valueDefinition' <|> exprDefinition'
  where
    exprDefinition' = fmap ExprDefinition exprDefinition
    valueDefinition' = fmap ValueDefinition valueDefinition

valueDefinition :: Parser ValueDefinition
valueDefinition = nameDefinition
  where
    nameDefinition =
      NameDefinition
        <$> (token Let *> name)
        <*> (token Assign *> expr)

exprDefinition :: Parser Expr
exprDefinition = noName expr 
  where
    noName :: Parser a -> Parser a
    noName p = Parser $ \case
      (t: ts) | t /= Let -> runParser p (t: ts)
      _ -> []


parensed :: Parser a -> Parser a
parensed p = token LParen *> p <* token RParen

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

opsL :: Parser (a -> a -> a) -> Parser a -> Parser a
opsL sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    squash = foldl' (\acc (combine, a) -> combine acc a)

opsR :: Parser (a -> a -> a) -> Parser a -> Parser a
opsR sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    shift (oldStart, stack) (combine, a) =
      (a, (combine, oldStart) : stack)

    squash start annotated =
      let (start', annotated') = foldl' shift (start, []) annotated
       in foldl' (\acc (combine, a) -> combine a acc) start' annotated'

expr :: Parser Expr
expr = letExpr <|> letInExpr <|> ifExpr <|> lambdaExpr <|> binExpr
  where
    ifExpr :: Parser Expr
    ifExpr = 
      IfExpr
        <$> (token If *> expr)
        <*> (token Then *> expr)
        <*> (token Else *> expr)

    lambdaExpr :: Parser Expr
    lambdaExpr = token Lambda *>
      liftA2 LambdaExpr (some name) (token Dot *> expr)

    letExpr :: Parser Expr
    letExpr =
      liftA2 LetExpr 
        (token Let *> name)
        (token Assign *> expr)

    letInExpr :: Parser Expr
    letInExpr = 
      LetInExpr
        <$> (token Let *> sepBy1 (liftA2 (,) name (token Assign *> expr)) (token Semicolon))
        <*> (token In *> expr)

    binExpr :: Parser Expr
    binExpr = pipe
      where
        pipe = opsL (lPipe <|> rPipe) compose
        lPipe = (BinOpExpr LPipeExpr <$ token LPipe)
        rPipe = (BinOpExpr RPipeExpr <$ token RPipe)

        compose = opsR (lCompose <|> rCompose) appExpr
        lCompose = (BinOpExpr LComposeExpr <$ token LCompose) 
        rCompose = (BinOpExpr RComposeExpr <$ token RCompose)

    appExpr :: Parser Expr
    appExpr = fmap extract $ some factor
      where
        extract [] = error "appExpr: No elements produced after some"
        extract [e] = e
        extract (e : es) = ApplyExpr e es

    factor :: Parser Expr
    factor = litExpr <|> nameExpr <|> parensed expr <|> bracketed expr
      where
        litExpr = fmap LitExpr literal
        nameExpr = fmap IdentifierExpr name

bracketed :: Parser Expr -> Parser Expr
bracketed p = ListExpr [] <$ emptyBracketed <|> ListExpr <$> nonEmptyBracketed
  where
    emptyBracketed :: Parser [Expr]
    emptyBracketed = token LBracket *> token RBracket *> pure []

    nonEmptyBracketed :: Parser [Expr]
    nonEmptyBracketed = token LBracket *> sepBy1 p (token Comma) <* token RBracket

ast :: Parser AST
ast = fmap AST (braced definition)

parse :: [Token] -> Either ParseError AST
parse input = case runParser ast input of
  [] -> Left FailedParser
  [(res, [])] -> Right res
  results -> Left $ AmbiguousParse results
