module Interpreter.Token where

data Token 
  = Let               -- let
  | If                -- if
  | Then              -- then
  | Else              -- else
  | Dot               -- .
  | Lambda            -- \ or λ
  | Assign            -- =
  | IntLit Int        -- 1, 2, 3, ...
  | BoolLit Bool      -- true, false
  | StringLit String  -- e.g."hello world"
  | Identifier String -- e.g. x, y, z, ...
  | LParen            -- (
  | RParen            -- )
  | LBrace            -- {
  | RBrace            -- }
  | Semicolon         -- ;
  -- TODO: Add these
  -- | LCompose          -- <<
  -- | RCompose          -- >>
  -- | LPipe             -- <|
  -- | RPipe             -- |>
  deriving (Show, Eq)

data RawToken
  = Blankspace String -- " "
  | Comment String    -- # ...
  | Newline           -- \n
  | NormalToken Token String
  deriving (Show, Eq)