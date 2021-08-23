module Scatterplot exposing (..)

import Html exposing (Html)
import Html.Attributes
import Scale exposing (ContinuousScale)
import Statistics
import Axis
import TypedSvg exposing (circle, g, text_, svg, style, rect)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))
import List.Extra

import Conflict
import Model exposing (Msg(..), MainViewType(..))
import Html.Events

scatterplot : List Conflict.Conflict -> Svg Msg
scatterplot filteredConflicts =
    let
        kreisbeschriftung : String
        kreisbeschriftung =
            ""

        xValues : List Float
        xValues =
            List.map (\c -> c.year |> toFloat) filteredConflicts
    
        yValues : List Float
        yValues =
            List.map (\c -> c.fatalities |> toFloat) filteredConflicts

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            Tuple.first t + (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ]
            , fontSize <| Px 15.0
            , fontFamily [ "sans-serif" ]
            ]
            [ xAxis xValues
            , text_ [ x (Scale.convert xScaleLocal labelPositions.x), y 30, textAnchor AnchorMiddle ] [ Html.text "Year" ]
            ]
        , g [ transform [ Translate (padding - 1) (padding) ]
            , fontSize <| Px 15.0
            , fontFamily [ "sans-serif" ]
            ]
            [ yAxis yValues
            , text_ [ x 0, y ((Scale.convert yScaleLocal labelPositions.y) - (padding/3)), textAnchor AnchorMiddle ] [ Html.text "Fatalities" ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) filteredConflicts)
        , g [ transform [ Translate padding padding ] ]
            (List.map (yearSelectionBox xScaleLocal yScaleLocal) (List.Extra.unique (List.map (.year) filteredConflicts)))
        ]

yearSelectionBox : ContinuousScale Float -> ContinuousScale Float -> Int -> Svg Msg
yearSelectionBox scaleX scaleY conflictYear =
    g
        [ class [ "yearSelection" ]
        , transform
            [ Translate
                ((Scale.convert scaleX (toFloat conflictYear)) - 13.5)
                0
            ]
        ]
        [ rect
            [ TypedSvg.Attributes.InPx.height 330.5
            , TypedSvg.Attributes.InPx.width 27
            , Html.Events.onClick (ChangeMainView (ParallelCoordinatesView conflictYear))
            ]
            []
        , rect
            [ class [ "textBox" ]
            , x 0
            , y 335
            , TypedSvg.Attributes.InPx.height 15
            , TypedSvg.Attributes.InPx.width 27
            , Html.Events.onClick (ChangeMainView (ParallelCoordinatesView conflictYear))
            ]
            []
        , text_
            [ x 2
            , y 346.5
            , Html.Events.onClick (ChangeMainView (ParallelCoordinatesView conflictYear))
            ]
            [ Html.text (String.fromInt conflictYear) ]
        ]

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