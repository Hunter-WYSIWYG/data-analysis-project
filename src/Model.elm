module Model exposing (..)

import Conflict
import Http
import TypedSvg.Types exposing (Filter)

init : () -> ( Model, Cmd Msg )
init flags =
    ( initModel, initCmd )

initModel : Model
initModel =
    { mainViewType = ScatterplotView
    , filterViewType = Region
    , conflicts = []
    , activeCountries = [ "Algeria" ]
    , activeFilter = emptyGeoTree
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
    , filterViewType : FilterViewType
    , conflicts : List Conflict.Conflict
    , activeCountries : List String
    , activeFilter : GeoTree
    }

type alias GeoTree =
    { regions : List String
    , countries : List String
    , locations : List String
    }

emptyGeoTree =
    { regions = []
    , countries = []
    , locations = []
    }

type Msg
    = GotData (Result Http.Error (List (Conflict.Conflict)))
    | UpdateSelectedCountries String
    | ChangeMainView MainViewType
    | ChangeFilterView FilterViewType

type MainViewType
    = ScatterplotView
    | ParallelCoordinatesView Int

type FilterViewType
    = Region
    | Country
    | Location