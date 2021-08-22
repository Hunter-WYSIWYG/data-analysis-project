module ParallelCoordinates exposing (..)

import Axis
import Html exposing (Html)
import Scale exposing (ContinuousScale, defaultBandConfig)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_, line, polygon)
import TypedSvg.Attributes exposing (points, class, fontFamily, fontSize, textAnchor, transform, viewBox, x1, y1, x2, y2, stroke, strokeWidth)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..), Paint(..))
import Color
import List.Extra

import Conflict

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

yAxisFloat : List Float -> Svg msg
yAxisFloat v =
    Axis.left
        [ Axis.tickCount tickCount
        ]
        (yScaleFloat v)

yScaleFloat : List Float -> ContinuousScale Float
yScaleFloat v =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent v )

yAxisOrdinal : List Int -> String -> Svg msg
yAxisOrdinal valueList axisName =
    let
        formatFunction =
            case axisName of
                "Month" -> intToMonth
                "Event type" -> intToEventType
                _ -> (\j -> "")
    in
    Axis.left [] (Scale.toRenderable formatFunction (yScaleOrdinal valueList axisName))

yScaleOrdinal : List Int -> String -> Scale.BandScale Int
yScaleOrdinal valueList axisName =
    let
        outerPadding =
            case axisName of
                "Month" -> 0.19
                "Event type" -> 0
                _ -> 0

        innerPadding =
            case axisName of
                "Month" -> 0
                "Event type" -> 0.36
                _ -> 0

        align =
            case axisName of
                "Month" -> 0.6
                "Event type" -> 0.5
                _ -> 0
    in
    Scale.band { defaultBandConfig | paddingInner = innerPadding, paddingOuter = outerPadding, align = align } ( h - 2 * padding, 0 ) valueList
