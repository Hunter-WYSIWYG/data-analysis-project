module Scatterplot exposing (..)

import Html exposing (Html)
import Scale exposing (ContinuousScale)
import Statistics
import Axis
import TypedSvg exposing (circle, g, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))

import Conflict

point : ContinuousScale Float -> ContinuousScale Float -> Conflict.Conflict -> Svg msg
point scaleX scaleY conflict =
    g   [ class [ "point" ]
        , fontSize <| Px 10.0
        , fontFamily [ "sans-serif" ]
        , transform
            [ Translate
                (Scale.convert scaleX (toFloat conflict.year))
                (Scale.convert scaleY (toFloat conflict.fatalities))
            ]
        ]
        [ circle [ cx 0, cy 0, r 5 ] []
        , text_ [ x 0, y 20, textAnchor AnchorMiddle ] [ Html.text conflict.notes ]
        ]

xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)

yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

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
wideExtent values =
    Maybe.withDefault
        defaultExtent
        (Maybe.map
            padExtent
            (Statistics.extent values)
        )

xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

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
    5