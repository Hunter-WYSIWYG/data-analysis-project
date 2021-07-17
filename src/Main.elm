module Main exposing (..)

import Browser
import Html
import Html.Attributes

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : () -> ( Model, Cmd Msg )
init flags =
  ( initModel, Cmd.none )

initModel : Model
initModel =
    { tmp = False
    }

type alias Model =
    { tmp : Bool
    }

type Msg
  = Tmp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tmp ->
        (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body = [ Html.text "init" ]
  }