module Main exposing (..)

import Browser 
import Element exposing (..)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Json.Decode as Decode
import Element.Input as Input

import Lib exposing (runTest)

main =
  Browser.sandbox { init = init, update = update, view = view }

onEnter : msg -> Element.Attribute msg
onEnter msg =
  Element.htmlAttribute
    (Html.Events.on "keyup"
      (Decode.field "key" Decode.string
        |> Decode.andThen
          (\key ->
            if key == "Enter" then
              Decode.succeed msg
            else
              Decode.fail "Not the enter key"
          )
      )
    )

getResult : String -> String
getResult expr =
  case runTest expr of
    Ok cont ->
      Debug.toString cont
    Err msg ->
      "ERROR: " ++ Debug.toString msg

type alias Model =
  { content : String
  , enterPressed : Bool
  , evalResult : String
  }

init : Model
init =
  { content = ""
  , enterPressed = False
  , evalResult = ""
  }

type Msg
  = Change String
  | EnterWasPressed

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }
    EnterWasPressed ->
      { model | enterPressed = True, evalResult = getResult model.content}

view : Model -> Html Msg
view model =
  Element.layout []
    ( Input.text 
      [ centerX
      , centerY
      , width (px 500)
      , spacing 16
      , onEnter EnterWasPressed
      , below
        (if model.enterPressed then
          el
            [ moveDown 5 ]
            (text model.evalResult)
        else
          Element.none
        )
      ]
      { onChange = Change
      , placeholder =
        Just 
          (Input.placeholder []
            (text "Type your expression")
          )
      , text = model.content
      , label = Input.labelAbove [] (text "Evaluate your LISP")
      }
    -- , div [] [ text (String.reverse model.content) ]
    )

