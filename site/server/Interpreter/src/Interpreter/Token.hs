module Interpreter.Token where

data Token 
  = Let               -- let
  | If                -- if
  | Then              -- then
  | Else              -- else
  | Dot               -- .
  | Lambda            -- \ or λ
  | Assign            -- =
  | IntLit Int        -- -3, -2, -1, 0, 1, 2, 3, ...
  | FloatLit Float    -- 1.0, 2.0, 3.0, ...
  | BoolLit Bool      -- true, false
  | StringLit String  -- e.g."hello world"
  | Identifier String -- e.g. x, y, z, ...
  | LParen            -- (
  | RParen            -- )
  | LBrace            -- {
  | RBrace            -- }
  | Semicolon         -- ;
  | LCompose          -- <<
  | RCompose          -- >>
  | LPipe             -- <|
  | RPipe             -- |>
  deriving (Show, Eq)

data RawToken
  = Blankspace String -- " "
  | Comment String    -- # ...
  | Newline           -- \n
  | NormalToken Token String
  deriving (Show, Eq)
