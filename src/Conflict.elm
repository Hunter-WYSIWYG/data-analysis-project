module Conflict exposing (..)

import Json.Decode
import Json.Decode.Extra

type alias Conflict =
    { year : Int
    , event_date : String
    , event_type : String
    , fatalities : Int
    , country : String
    , region : String
    }

listDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
listDecoder decoder =
    Json.Decode.list decoder

decodeConflict : Json.Decode.Decoder Conflict
decodeConflict =
    Json.Decode.succeed Conflict
        |> Json.Decode.Extra.andMap (Json.Decode.field "YEAR" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "EVENT_DATE" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "EVENT_TYPE" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "FATALITIES" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "COUNTRY" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "REGION" Json.Decode.string)