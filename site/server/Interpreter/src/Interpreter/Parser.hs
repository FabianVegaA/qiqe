{-# LANGUAGE LambdaCase #-}

module Interpreter.Parser where

import Control.Applicative (Alternative (..), liftA2)
import Data.Bifunctor (first)
import Interpreter.Token (Token (..))

data Expr
  = LitExpr Literal
  | IdentifierExpr ValName
  | LambdaExpr [ValName] Expr
  | ApplyExpr Expr [Expr]
  | LetExpr ValName Expr
  | IfExpr Expr Expr Expr
  deriving (Show, Eq)

type ValName = String

data Literal 
  = IntLitExpr Int
  | BoolLitExpr Bool
  | StringLitExpr String
  deriving (Show, Eq, Ord)

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
literal = intLit <|> stringLit <|> boolLit
  where
    intLit :: Parser Literal
    intLit = pluck $ \case 
      IntLit i -> Just (IntLitExpr i)
      _ -> Nothing

    stringLit :: Parser Literal
    stringLit = pluck $ \case
      StringLit s -> Just (StringLitExpr s)
      _ -> Nothing

    boolLit :: Parser Literal
    boolLit = pluck $ \case
      BoolLit b -> Just (BoolLitExpr b)
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

expr :: Parser Expr
expr = ifExpr <|> appExpr <|> lambdaExpr <|> letExpr
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

    appExpr :: Parser Expr
    appExpr = fmap extract $ some factor
      where
        extract [] = error "appExpr: No elements produced after some"
        extract [e] = e
        extract (e : es) = ApplyExpr e es

    factor :: Parser Expr
    factor = littExpr <|> nameExpr <|> parensed expr
      where
        littExpr = fmap LitExpr literal
        nameExpr = fmap IdentifierExpr name


ast :: Parser AST
ast = fmap AST (braced definition)

parse :: [Token] -> Either ParseError AST
parse input = case runParser ast input of
  [] -> Left FailedParser
  [(res, [])] -> Right res
  results -> Left $ AmbiguousParse results
