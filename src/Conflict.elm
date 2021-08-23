module Conflict exposing (..)

import Json.Decode
import Json.Decode.Extra

type alias Conflict =
    { event_id_no_cnty : Int
    , year : Int
    , event_date : String
    , event_type : String
    , fatalities : Int
    , notes : String
    , country : String
    , region : String
    , location : String
    }

listDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
listDecoder decoder =
    Json.Decode.list decoder

decodeConflict : Json.Decode.Decoder Conflict
decodeConflict =
    Json.Decode.succeed Conflict
        |> Json.Decode.Extra.andMap (Json.Decode.field "EVENT_ID_NO_CNTY" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "YEAR" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "EVENT_DATE" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "EVENT_TYPE" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "FATALITIES" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "NOTES" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "COUNTRY" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "REGION" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "LOCATION" Json.Decode.string)