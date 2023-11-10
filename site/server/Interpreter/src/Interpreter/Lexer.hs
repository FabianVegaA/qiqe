{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter.Lexer (lexer) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, gets, modify', runState)
import Data.Char (isAlphaNum, isDigit, isSpace, isLower)
import Data.Bifunctor (first)
import Data.List (foldl1', foldl')
import Data.Maybe (listToMaybe, maybeToList)
import Interpreter.Token (Token (..), RawToken (..))

-- TODO: https://cronokirby.com/posts/2020/12/haskell-in-haskell-2

data LexerError 
  = UnexpectedChar Char
  | UnexpectedEOF
  | UnmatchedLayout
  | UnimplementedError
  deriving (Show, Eq)

newtype Lexer a = Lexer {
    runLexer :: String -> Either LexerError (a, String)
  }

instance Functor Lexer where
  fmap f (Lexer l) = Lexer (fmap (first f) . l) 

instance Applicative Lexer where
  pure a = Lexer $ \ s -> Right (a, s)
  (Lexer f) <*> (Lexer a) = Lexer $ \ s -> do
    (f', s') <- f s
    (a', s'') <- a s'
    pure (f' a', s'')

instance Alternative Lexer where
  empty = Lexer (Left . unexpected)
  (Lexer a) <|> (Lexer b) = Lexer $ \ input ->
    case (a input, b input) of
      (res, Left _) -> res
      (Left _, res) -> res
      (a'@(Right (_, restA)), b'@(Right (_, restB))) ->
        if length restA <= length restB
          then a'
          else b'

data LinePosition =  Start | Middle deriving (Show, Eq)

data Positioned a = Positioned a LinePosition Int deriving (Show, Eq)

type PosState = (LinePosition, Int)

data Layout = Explicit | Implicit Int

data LayoutState = LayoutState
  { layouts :: [Layout],
    tokens :: [Token],
    expectingLayout :: Bool
  }

type LayoutM a = ExceptT LexerError (State LayoutState) a

runLayoutM :: LayoutM a -> Either LexerError [Token]
runLayoutM =
  runExceptT >>> (`runState` LayoutState [] [] True) >>> \case
    (Left e, _) -> Left e
    (Right _, LayoutState _ ts _) -> Right (reverse ts)
  where
    f >>> g = g . f

yieldToken :: Token -> LayoutM ()
yieldToken t = modify' (\s -> s {tokens = t : tokens s})

pushLayout :: Layout -> LayoutM ()
pushLayout l = modify' (\s -> s {layouts = l : layouts s})

popLayout :: LayoutM ()
popLayout = modify' (\s -> s {layouts = drop 1 (layouts s)})

currentLayout :: LayoutM (Maybe Layout)
currentLayout = fmap listToMaybe $ gets layouts 

compareIndentation :: Int -> LayoutM Ordering
compareIndentation col =
  let cmp Nothing = GT
      cmp (Just Explicit) = GT
      cmp (Just (Implicit n)) = compare col n
   in fmap cmp currentLayout

layout :: [Positioned Token] -> Either LexerError [Token]
layout inputs = runLayoutM $ do
    mapM_ step inputs
    closeImplicitLayouts
  where
    step :: Positioned Token -> LayoutM ()
    step (Positioned t linePos col) = do
      expectingLayout' <- gets expectingLayout
      case t of
        RBrace -> closeExplicitLayout
        LBrace | expectingLayout' -> startExplicitLayout
        _
          | expectingLayout' -> startImplicitLayout col
          | linePos == Start -> continueImplicitLayout col
          | otherwise -> return ()
      yieldToken t

    closeExplicitLayout :: LayoutM ()
    closeExplicitLayout =
      currentLayout >>= \case
        Just Explicit -> popLayout
        _ -> throwError (UnexpectedChar '}')

    startExplicitLayout :: LayoutM ()
    startExplicitLayout = do
      modify' (\s -> s {expectingLayout = False})
      pushLayout Explicit

    startImplicitLayout :: Int -> LayoutM ()
    startImplicitLayout col = do
      modify' (\s -> s {expectingLayout = False})
      compareIndentation col >>= \case
        GT -> do
          yieldToken LBrace
          pushLayout (Implicit col)
        _ -> do
          yieldToken LBrace
          yieldToken RBrace
          continueImplicitLayout col

    continueImplicitLayout :: Int -> LayoutM ()
    continueImplicitLayout col = do
      closeFurtherLayouts
      compareIndentation col >>= \case
        EQ -> yieldToken Semicolon
        _ -> return ()
      where
        closeFurtherLayouts =
          compareIndentation col >>= \case
            LT -> do
              yieldToken RBrace
              popLayout
              closeFurtherLayouts
            _ -> return ()

    closeImplicitLayouts :: LayoutM ()
    closeImplicitLayouts =
      currentLayout >>= \case
        Nothing -> return ()
        Just Explicit -> throwError UnmatchedLayout
        Just (Implicit _) -> do
          yieldToken RBrace
          popLayout
          closeImplicitLayouts

position :: [RawToken] -> [Positioned Token]
position = reverse . snd . foldl' go ((Start, 0), [])
  where
    go :: (PosState, [Positioned Token]) -> RawToken -> (PosState, [Positioned Token])
    go (p, acc) raw =
      let (p', produced) = eat p raw
      in (p', maybeToList produced <> acc)

eat :: PosState -> RawToken -> (PosState, Maybe (Positioned Token))
eat (pos, col) = \case
  Newline -> ((Start, 0), Nothing)
  Comment _ -> ((Start, 0), Nothing)
  Blankspace s -> ((pos, col + length s), Nothing)
  NormalToken t s -> ((Middle, col + length s), Just (Positioned t pos col))

rawLexer :: Lexer [RawToken]
rawLexer = some (whitespace <|> comment <|> rawToken)
  where 
    rawToken = fmap (uncurry NormalToken) token

    comment = Comment <$> (string "#" *> many (satisfies (/= '\n')))

    whitespace = blankspace <|> newline

    blankspace = Blankspace <$> some (satisfies (\c -> isSpace c && c /= '\n'))

    newline = Newline <$ char '\n'

lexer :: String -> Either LexerError [Token]
lexer input = do 
  res <- runLexer rawLexer input
  layout . position . fst $ res

unexpected :: String -> LexerError
unexpected [] = UnexpectedEOF
unexpected (c:_) = UnexpectedChar c

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p = Lexer $ \ s -> case s of
  (c:cs) | p c -> Right (c, cs)
  rest -> Left $ unexpected rest

char :: Char -> Lexer Char
char c = satisfies (== c)

string :: String -> Lexer String
string = traverse char

oneOf :: Alternative f => [f a] -> f a
oneOf = foldl1' (<|>)

token :: Lexer (Token, String)
token = oneOf
  [ keyword
  , literal
  , name
  , operator
  ]
  where 
    with :: Functor f => b -> f a -> f (b, a)
    with b = fmap (b,)

    keyword :: Lexer (Token, String)
    keyword = oneOf
      [ Let `with` string "let"
      , If `with` string "if"
      , Then `with` string "then"
      , Else `with` string "else"
      ]

    operator :: Lexer (Token, String)
    operator = oneOf
      [ Dot `with` string "."
      , Lambda `with` oneOf [string "\\", string "λ", string "\955"]
      , Assign `with` string "="
      , LParen `with` string "("
      , RParen `with` string ")"
      , LBrace `with` string "{"
      , RBrace `with` string "}"
      , LCompose `with` string "<<"
      , RCompose `with` string ">>"
      , LPipe `with` string "<|"
      , RPipe `with` string "|>"
      ]
    
    literal :: Lexer (Token, String)
    literal = intLit <|> floatLit <|> boolLit <|> stringLit
      where
        intLit :: Lexer (Token, String)
        intLit = fmap (\ n -> (IntLit (read n), n)) $ let
          sign = string "-" <|> string ""
          digits = some (satisfies isDigit)
          in liftA2 (++) sign digits

        floatLit :: Lexer (Token, String)
        floatLit = fmap (\ n -> (FloatLit (read n), n)) $ let
          sign = string "-" <|> string ""
          digits = some (satisfies isDigit)
          dot = char '.'
          in liftA2 (++) sign (liftA2 (++) digits (liftA2 (:) dot digits))

        boolLit :: Lexer (Token, String)
        boolLit = BoolLit True `with` string "true" <|> BoolLit False `with` string "false"

        stringLit :: Lexer (Token, String)
        stringLit = fmap (\s -> (StringLit s, s)) $ (char '"' *> many (satisfies (/= '"')) <* char '"')

    name :: Lexer (Token, String)
    name = identifier
      where
        identifier :: Lexer (Token, String)
        identifier = fmap (\i-> (Identifier i, i)) $ (satisfies isLower `followedBy` (continueName <|> altName))
        
        continueName :: Lexer Char
        continueName = satisfies (\c -> isAlphaNum c || c == '_')

        altName :: Lexer Char
        altName = satisfies (\c -> c == '\'' || c == '_')

        followedBy :: Lexer Char -> Lexer Char -> Lexer String
        followedBy p1 p2 = liftA2 (:) p1 (many p2)
