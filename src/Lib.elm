module Lib exposing (..)
import Parser exposing (..)
import Char exposing (isAlpha, isAlphaNum)
import Set
import Dict exposing (Dict, empty)

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

type Op
  = Add
  | Mul

type SExp
  = Number Int
  | Boolean Bool
  | Identifier String
  | List (List SExp)
  | Operator (Op)

-- parseNothing : Parser ()
-- parseNothing =
--   succeed ()

blankSpaceParser : Parser ()
blankSpaceParser =
  loop 0 <| ifProgress <|
    oneOf
      [ lineComment "--"
      , multiComment "[-" "-]" Nestable
      , spaces
      ]

symbolParser : String -> Parser String
symbolParser a =
  getChompedString <|
    succeed identity
    |. blankSpaceParser
    |= symbol a
    |. blankSpaceParser

-- parseResult : Parser a -> String -> Result (List DeadEnd) a
-- parseResult parser value =
--   run parser value

-- getStringResult : Result (List DeadEnd) String -> String
-- getStringResult result =
--   case result of
--     Ok value ->
--       value
--     Err error ->
--       deadEndsToString error


ifProgress : Parser a -> Int -> Parser (Step Int ())
ifProgress parser offset =
  succeed identity
    |. parser
    |= getOffset
    |> map (\newOffset -> if offset == newOffset then Done () else Loop newOffset)

number : Parser Int
number =
  oneOf
    [ succeed negate
      |. symbol "-"
      |= int
    , succeed identity
      |. symbol "+"
      |= int
    , int
    ]

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
    { start = \c -> isAlpha c || c == '_'
    , inner = \c -> isAlphaNum c || c == '_' || c == '-'
    , reserved = Set.fromList ["lambda", "if", "begin", "define", "let", "and", "or"]
    }

operator : Parser Op
operator =
  oneOf 
    [ succeed Add 
      |. symbolParser "+"
    , succeed Mul 
      |. symbolParser "*"
    ]


-- list : Parser (List SExp)
-- list =
--   Parser.Advanced.sequence
--     { start = }

-- list : Parser (List SExp)
-- list =
--   sequence
--     { start = getStringResult (parseResult symbolParser "(")
--     , separator = 
--     , end = ")"
--     , spaces = parseNothing
--     , item = lazy (\_ -> sExp)
--     , trailing = Forbidden
--     }

list : Parser (List SExp)
list =
  succeed (\exp -> exp)
    |. symbolParser "("
    |= loop [] sExpsParser
    |. symbolParser ")"

sExp : Parser SExp
sExp =
  oneOf
    [ map Number ( backtrackable number )
    , map Operator operator
    , map Boolean bool
    , map List list
    , map Identifier identifier
    ]

sExpsParser : List SExp -> Parser (Step (List SExp) (List SExp))
sExpsParser sExps =
  oneOf
    [ succeed (\expr -> Loop (expr :: sExps))
      |= sExp
      |. spaces
    , succeed ()
      |> map (\_ -> Done (List.reverse sExps))
    ]

runTest : String -> Result (List DeadEnd) (List SExp)
runTest inp =
  run list inp

type alias Model = Dict String SExp

init : Model
init = empty

-- eval : 


-- type alias VarTable = Dict String SExp

-- eval : SExp -> SExp
-- eval expr = case expr of
--     Number n -> Number n
--     Boolean b -> Boolean b
--     Operator op -> Operator op
--     Identifier id -> Identifier id
--     List ses -> List ses

-- type Error
--   = UndefinedVariable String

-- type alias Test = Maybe String

-- eval : SExp -> State VarTable (Result Error SExp) 
-- -- eval : SExp -> State VarTable (Maybe SExp) 
-- eval expr = case expr of
--     Number n -> State.state (Result.Ok (Number n))
--     Boolean b -> State.state (Result.Ok (Boolean b))
--     Operator op -> State.state (Result.Ok (Operator op))
--     Identifier id -> 
--       State.join
--       |> case State.run (Dict.get id) of
--         Nothing -> Err UndefinedVariable id
--         Just x -> x
--     List ses -> List ses






-- Identifier id -> gets (Map.lookup id) >>= \case
--   Nothing -> throwError $ UndefinedVariable id
--   Just x -> return x
