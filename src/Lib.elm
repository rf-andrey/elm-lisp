module Lib exposing (..)
import Parser exposing (..)

{-
  - Números
  - Booleanos
  - Listas
  - Definição de variáveis
  - Lambdas
  - Invocar funções
-}

type Op
  = Add
  | Mul

type SExp
  = Number Int
  | Boolean Bool
  | Identifier String
  | List (List SExp)
  | Operator (Op)