module Lib exposing (..)
import Parser exposing (..)
-- import Parser.Advanced as Adv
import Char
import Set
import Html exposing (a)

{-
  - Números
  - Booleanos
  - Listas
  - Definição de variáveis
  - Lambdas
  - Invocar funções
-}

-- > (+ 1 2)
-- 3

type SExp
  = Number Int
  | Boolean Bool
  | Identifier String
  | List (List SExp)

number : Parser Int
number =
  succeed identity
    |= int

bool : Parser Bool
bool =
  succeed identity
  |. symbol "#"
  |= oneOf
    [
      map (\_ -> True) (keyword "t"),
      map (\_ -> False) (keyword "f")
    ]

identifier : Parser String
identifier =
  variable
    { start = \c -> Char.isAlpha c || c == '_'
    , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
    , reserved = Set.fromList ["lambda", "if", "begin", "define", "let", "and", "or"]
    }

