module ParallelCoordinates exposing (..)

import Statistics

w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    8

segmentDistance : Float
segmentDistance =
    200

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

padExtent : ( Float, Float ) -> ( Float, Float )
padExtent ( min, max ) =
    let
        -- extentPadding = 0.1 * (max - min)

        extentPadding = (max - min) / toFloat (tickCount * 2) 
    in
    ( min - extentPadding, max + extentPadding )


wideExtent : List Float -> ( Float, Float )
wideExtent v =
    Maybe.withDefault
        defaultExtent
        (Maybe.map
            padExtent
            (Statistics.extent v)
        )