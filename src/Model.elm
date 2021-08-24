module Model exposing (..)

import Conflict
import Http
import TypedSvg.Types exposing (Filter)
import Dict exposing (Dict)

init : () -> ( Model, Cmd Msg )
init flags =
    ( initModel, initCmd )

initModel : Model
initModel =
    { mainViewType = ScatterplotView
    , conflicts = []
    , activeFilter = initFilter
    , showGeoLocationName = " "
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
    { mainViewType : MainViewType
    , conflicts : List Conflict.Conflict
    , activeFilter : Filter
    , showGeoLocationName : String
    }

type alias Filter =
    { regions : List String
    , countries : List String
    }

type alias GeoTree =
    { regions : List String --no Key necessary -> root is "Afrika"
    , countries : Dict String (List String) --Key (Region) and Values (Countries)
    }

initFilter : Filter
initFilter =
    { regions = [ "Western Africa", "Southern Afrika" ]
    , countries = [ "Ghana" ]
    }

type Msg
    = GotData (Result Http.Error (List (Conflict.Conflict)))
    | ChangeMainView MainViewType
    | UpdateActiveFilter (Maybe GeoLocationType) String
    | ShowGeoLocationName String

type MainViewType
    = ScatterplotView
    | ParallelCoordinatesView Int

type GeoLocationType
    = Region
    | Country