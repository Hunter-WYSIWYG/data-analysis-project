module Tree exposing (..)

import Html
import Html.Events
import Html.Attributes
import Svg exposing (Svg, line, circle, text_, g)
import Svg.Attributes exposing (x1, y1, x2, y2, stroke, textAnchor, transform)
import TypedSvg exposing (style)
import TreeDiagram exposing (node, Tree, defaultTreeLayout, leftToRight)
import TreeDiagram.Svg exposing (draw)

import Model exposing (Msg(..), FilterType(..), GeoTree)
import Dict exposing (Dict)
import Svg exposing (rect)
import TypedSvg.Core

renderTree : GeoTree -> Svg Msg
renderTree geoTree =
    let
        tree = buildTree geoTree
    in
    draw { defaultTreeLayout | orientation = leftToRight } drawNode drawLine tree

buildTree : GeoTree -> Tree (Maybe FilterType, String)
buildTree geoTree =
    let
        regions = geoTree.regions
        countries = geoTree.countries
        locations = geoTree.locations
        locationNodes =
            Dict.map
                (\key locNames ->
                    List.map
                        (\locName ->
                            node
                                (Just Location, locName)
                                []
                        )
                        locNames
                )
                locations
        countryNodes =
            Dict.map
                (\key countryNames ->
                    List.map
                        (\countryName ->
                            node
                                (Just Country, countryName)
                                (Maybe.withDefault [] (Dict.get countryName locationNodes))
                        )
                        countryNames
                )
                countries
        regionNodes =
            List.map
                (\r ->
                    node
                        (Just Region, r)
                        (Maybe.withDefault [] (Dict.get r countryNodes))
                )
                regions
    in
    node
        (Nothing, "Africa")
        regionNodes

toString : (String -> a) -> Float -> a
toString prop value =
    prop (String.fromFloat value)

drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ toString x1 0, toString y1 0, (toString x2 targetX), toString y2 targetY, stroke "black" ]
        []

drawNode : (Maybe FilterType, String) -> Svg Msg
drawNode (maybeFilterType, name) =
    let
        rootWidth = 60
        regionWidth = 100
        countryWidth = 80
        locationWidth = 80
        widthString =
            case maybeFilterType of
                Just Region -> String.concat [ (String.fromInt regionWidth), "px" ]
                Just Country -> String.concat [ (String.fromInt countryWidth), "px" ]
                Just Location -> String.concat [ (String.fromInt locationWidth), "px" ]
                Nothing -> String.concat [ (String.fromInt rootWidth), "px" ]
        offsetString =
            case maybeFilterType of
                Just Region -> String.concat [ (String.fromInt (floor (negate (regionWidth/2)))), "px" ]
                Just Country -> String.concat [ (String.fromInt (floor (negate (countryWidth/2)))), "px" ]
                Just Location -> String.concat [ (String.fromInt (floor (negate (locationWidth/2)))), "px" ]
                Nothing -> String.concat [ (String.fromInt (floor (negate (rootWidth/2)))), "px" ]
    in
    g
        [ Svg.Attributes.class "treeNodeBox" ]
        [ text_ [ textAnchor "middle", transform "translate(0,5) rotate(0 0 0)" ] [ Html.text name ]
        , rect
            [ Svg.Attributes.height "20px"
            , Svg.Attributes.width widthString
            , Svg.Attributes.x offsetString
            , Svg.Attributes.y "-10px"
            , Html.Events.onClick (UpdateActiveFilter maybeFilterType name)
            ] []
        ]