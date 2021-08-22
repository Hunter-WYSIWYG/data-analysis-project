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

parallelCoordinates : List Conflict.Conflict -> Int -> Svg msg
parallelCoordinates conflicts year =
    let
        half : ( Float, Float ) -> Float
        half t =
            Tuple.first t + (Tuple.second t - Tuple.first t) / 2
            
        achsen =
            List.indexedMap
                (\i axisName ->
                    case axisName of
                        "Fatalities" ->
                            g
                                [ transform [ Translate (padding + ((toFloat i)*segmentDistance) - 1) (padding - 1) ]
                                , fontSize <| Px 15.0
                                , fontFamily [ "sans-serif" ]
                                ]
                                [ yAxisFloat (values axisName conflicts)
                                , text_ [ x 0, y (-20), textAnchor AnchorMiddle ] [ Html.text axisName ]
                                ]
                        _ ->
                            let
                                scaleList =
                                    case axisName of
                                        "Month" -> List.range 1 12
                                        "Event type" -> List.range 1 6
                                        "Region" -> List.range 1 5
                                        _ -> []
                            in
                            g
                                [ transform [ Translate (padding + ((toFloat i)*segmentDistance) - 1) (padding - 1) ]
                                , fontSize <| Px 15.0
                                , fontFamily [ "sans-serif" ]
                                ]
                                [ yAxisOrdinal scaleList axisName
                                , text_ [ x 0, y (-20), textAnchor AnchorMiddle ] [ Html.text axisName ]
                                ]
                )
                dimensionNames

        description =
            g
                [ transform [ Translate (padding) (padding) ]
                , fontSize <| Px 15.0
                , fontFamily [ "sans-serif" ]
                ]
                [ text_ [ x -20, y -40 ] [ Html.text (String.concat ["Year: ", (String.fromInt year)]) ]
                ]
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        (   achsen
            ++
            drawAllSegments conflicts
            ++
            [ description ]
        )