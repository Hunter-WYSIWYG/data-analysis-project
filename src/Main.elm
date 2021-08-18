module Main exposing (..)

import Browser
import Html
import Html.Attributes
import Http

import Conflict

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
    ( initModel, initCmd )

initModel : Model
initModel =
    { conflicts = []
    }

initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ Http.get
            { url = "data/Africa-Conflict_1997-2020.json" --"https://cors-anywhere.herokuapp.com/https://cloud.uzi.uni-halle.de/owncloud/index.php/s/jOcq5Jcf2E8zVhJ/download"
            , expect = Http.expectJson GotData (Conflict.listDecoder Conflict.decodeConflict)
            }
        ]

type alias Model =
    { conflicts : List Conflict.Conflict
    }

type Msg
    = GotData (Result Http.Error (List (Conflict.Conflict)))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok newConflicts ->
                    ( { model | conflicts = newConflicts }, Cmd.none)

                Err _ ->
                    ( { model | conflicts = [] }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body = [ Html.text "init" ]
  }