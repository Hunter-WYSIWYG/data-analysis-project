module Model exposing (..)

import Conflict
import Http

init : () -> ( Model, Cmd Msg )
init flags =
    ( initModel, initCmd )

initModel : Model
initModel =
    { viewType = ScatterplotView
    , conflicts = []
    , scatterplotCountries = []
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
    { viewType : ViewType
    , conflicts : List Conflict.Conflict
    , scatterplotCountries : List String
    }

type Msg
    = GotData (Result Http.Error (List (Conflict.Conflict)))
    | UpdateSelectedCountries String
    | ChangeView ViewType

type ViewType
    = ScatterplotView
    | ParallelCoordinatesView Int
    | TreeView