module Scatterplot exposing (..)

import Html exposing (Html)
import Html.Attributes
import Scale exposing (ContinuousScale)
import Statistics
import Axis
import TypedSvg exposing (circle, g, text_, svg, style)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))
import List.Extra

import Conflict

scatterplot : List Conflict.Conflict -> List Conflict.Conflict -> Svg msg
scatterplot conflictsForAxes filteredConflicts =
    let
        kreisbeschriftung : String
        kreisbeschriftung =
            ""

        xValues : List Float
        xValues =
            List.map (\c -> c.year |> toFloat) conflictsForAxes
    
        yValues : List Float
        yValues =
            List.map (\c -> c.fatalities |> toFloat) conflictsForAxes

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
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate (padding - 1) (h - padding) ] 
            , fontSize <| Px 15.0
            , fontFamily [ "sans-serif" ]
            ]
            [ xAxis xValues
            , text_ [ x (Scale.convert xScaleLocal labelPositions.x), y 30, textAnchor AnchorMiddle ] [ Html.text "Year" ]
            ]
        , g
            [ transform [ Translate (padding - 1) (padding - 1) ]
            , fontSize <| Px 15.0
            , fontFamily [ "sans-serif" ]
            ]
            [ yAxis yValues
            , text_ [ x 0, y ((Scale.convert yScaleLocal labelPositions.y) - (padding/3)), textAnchor AnchorMiddle ] [ Html.text "Fatalities" ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) filteredConflicts)
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

newScatterplotCountries : List String -> String -> List String
newScatterplotCountries oldCountries newCountry =
    if (List.member newCountry oldCountries) then
        List.Extra.remove newCountry oldCountries
    else
        newCountry::oldCountries

filterConflictForCountries : List Conflict.Conflict -> List String -> List Conflict.Conflict
filterConflictForCountries conflicts countries =
    List.filter (\conflict -> (List.member conflict.country countries)) conflicts