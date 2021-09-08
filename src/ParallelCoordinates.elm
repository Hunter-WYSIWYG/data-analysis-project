module ParallelCoordinates exposing (..)

import Axis
import Html exposing (Html)
import Html.Attributes
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
    500


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

yAxisOrdinal : String -> Svg msg
yAxisOrdinal axisName =
    let
        formatFunction =
            case axisName of
                "Month" -> intToMonth
                "Event type" -> intToEventType
                _ -> (\j -> "")
    in
    Axis.left [] (Scale.toRenderable formatFunction (yScaleOrdinal axisName))

yScaleOrdinal : String -> Scale.BandScale Int
yScaleOrdinal axisName =
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
    Scale.band { defaultBandConfig | paddingInner = innerPadding, paddingOuter = outerPadding, align = align } ( h - 2 * padding, 0 ) (idList axisName)

renderParallelCoordinates : List Conflict.Conflict -> Int -> Svg msg
renderParallelCoordinates conflicts year =
    let
        filteredConflicts =
            List.filter (\c -> c.year == year) conflicts

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
                                [ yAxisFloat (values axisName filteredConflicts)
                                , text_ [ x 0, y (-20), textAnchor AnchorMiddle ] [ Html.text axisName ]
                                ]
                        _ ->
                            g
                                [ transform [ Translate (padding + ((toFloat i)*segmentDistance) - 1) (padding - 1) ]
                                , fontSize <| Px 15.0
                                , fontFamily [ "sans-serif" ]
                                ]
                                [ yAxisOrdinal axisName
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
    svg [ viewBox 0 0 w h
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 70
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "margin" "auto"
        ]
        (   achsen
            ++
            drawAllSegments filteredConflicts
            ++
            [ description ]
        )

drawAllSegments : List Conflict.Conflict -> List (Svg msg)
drawAllSegments conflicts =
    let
        keyList1 = List.reverse (List.drop 1 (List.reverse dimensionNames))
        keyList2 = (List.drop 1 dimensionNames)
    in
    List.concat
        (List.indexedMap
            (\i k1 ->
                let
                    k2 = Maybe.withDefault "" (getElem i keyList2)
                in
                drawOneSegment k1 k2 conflicts i
            )
            keyList1
        )

getElem : Int -> List a -> Maybe a
getElem i list =
    List.head (List.drop i list)

drawOneSegment : String -> String -> List Conflict.Conflict -> Int -> List (Svg msg)
drawOneSegment key1 key2 conflicts segmentIndex =
    let
        values1 = values key1 conflicts
        values2 = values key2 conflicts
    in
    List.map2
        (\v1 v2 ->
            drawLine key1 key2 v1 v2 conflicts segmentIndex
        )
        values1
        values2

drawLine : String -> String -> Float -> Float -> List Conflict.Conflict -> Int -> Svg msg
drawLine key1 key2 value1 value2 conflicts segmentIndex =
    line
        [ x1 <| Px (padding + ((toFloat segmentIndex) * segmentDistance) - 1)
        , y1 <| Px ((Scale.convert (yScaleLocal key1 conflicts) value1) + padding)
        , x2 <| Px (padding + ((toFloat (segmentIndex+1)) * segmentDistance) - 1)
        , y2 <| Px ((Scale.convert (yScaleLocal key2 conflicts) value2) + padding)
        , strokeWidth <| Px 0.5
        , stroke <| Paint <| Color.rgba 0 0 0 1
        ]
        []

yScaleLocal : String -> List Conflict.Conflict -> ContinuousScale Float
yScaleLocal key conflicts =
    let
        scaleList =
            case key of
                "Month" -> List.map (toFloat) (idList "Month")
                "Event type" -> List.map (toFloat) (idList "Event type")
                _ -> values key conflicts
    in
    yScaleFloat scaleList

values : String -> List Conflict.Conflict -> List Float
values key conflicts =
    case key of
        "Event type" ->
            List.map
                (\c -> toFloat (eventTypeToInt c.event_type))
                conflicts
        "Fatalities" ->
            List.map
                (\c -> toFloat c.fatalities)
                conflicts
        "Month" ->
                List.map
                (\c -> toFloat (monthToInt (dateStringToMonthString c.event_date)))
                conflicts
        _ -> []

dimensionNames : List String
dimensionNames =
    [ "Month"
    , "Fatalities"
    , "Event type"
    ]

idList : String -> List Int
idList axisName =
    case axisName of
        "Month" -> List.range 1 12
        "Event type" -> List.range 1 6
        _ -> []

eventTypeToInt : String -> Int
eventTypeToInt eventType =
    case eventType of
        "Violence against civilians" -> 1
        "Battles" -> 2
        "Explosions/Remote violence" -> 3
        "Protests" -> 4
        "Strategic developments" -> 5
        "Riots" -> 6
        _ -> 0

intToEventType : Int -> String
intToEventType eventType =
    case eventType of
        1 -> "Violence against civilians"
        2 -> "Battles"
        3 -> "Explosions/Remote violence"
        4 -> "Protests"
        5 -> "Strategic developments"
        6 -> "Riots"
        _ -> "undefined"

dateStringToMonthString : String -> Maybe String
dateStringToMonthString date =
    if (String.contains "janvier" date) then Just "Jan" else
    if (String.contains "février" date) then Just "Feb" else
    if (String.contains "mars" date) then Just "Mar" else
    if (String.contains "avril" date) then Just "Apr" else
    if (String.contains "mai" date) then Just "May" else
    if (String.contains "juin" date) then Just "June" else
    if (String.contains "juillet" date) then Just "July" else
    if (String.contains "août" date) then Just "Aug" else
    if (String.contains "septembre" date) then Just "Sep" else
    if (String.contains "octobre" date) then Just "Oct" else
    if (String.contains "novembre" date) then Just "Nov" else
    if (String.contains "décembre" date) then Just "Dec" else Nothing

monthToInt : Maybe String -> Int
monthToInt month =
    case month of
        Just "Jan" -> 1
        Just "Feb" -> 2
        Just "Mar" -> 3
        Just "Apr" -> 4
        Just "May" -> 5
        Just "June" -> 6
        Just "July" -> 7
        Just "Aug" -> 8
        Just "Sep" -> 9
        Just "Oct" -> 10
        Just "Nov" -> 11
        Just "Dec" -> 12
        _ -> 0

intToMonth : Int -> String
intToMonth monthID =
    case monthID of
        1 -> "Jan"
        2 -> "Feb"
        3 -> "Mar"
        4 -> "Apr"
        5 -> "May"
        6 -> "June"
        7 -> "July"
        8 -> "Aug"
        9 -> "Sep"
        10 -> "Oct"
        11 -> "Nov"
        12 -> "Dec"
        _ -> "undefined"