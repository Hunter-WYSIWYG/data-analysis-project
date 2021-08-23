module Tree exposing (..)

import Html
import Svg exposing (Svg, line, circle, text_, g)
import Svg.Attributes exposing (..)
import TreeDiagram exposing (node, Tree, defaultTreeLayout, leftToRight)
import TreeDiagram.Svg exposing (draw)

import Model exposing (Msg(..))
import Model exposing (GeoTree)
import Dict
import Dict exposing (Dict)

renderTree : GeoTree -> Svg Msg
renderTree geoTree =
    let
        tree = buildTree geoTree
    in
    draw { defaultTreeLayout | orientation = leftToRight } drawNode drawLine tree

buildTree : GeoTree -> Tree String
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
                            node locName []
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
                                countryName
                                (Maybe.withDefault [] (Dict.get countryName locationNodes))
                        )
                        countryNames
                )
                countries
        regionNodes =
            List.map
                (\r ->
                    node
                        r
                        (Maybe.withDefault [] (Dict.get r countryNodes))
                )
                regions
    in
    node
        "Africa"
        regionNodes

toString : (String -> a) -> Float -> a
toString prop value =
    prop (String.fromFloat value)

drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ toString x1 0, toString y1 0, (toString x2 targetX), toString y2 targetY, stroke "black" ]
        []

drawNode : String -> Svg msg
drawNode n =
    g
        []
        [ circle [ r "16", stroke "black", fill "white", cx "0", cy "0" ] []
        , text_ [ textAnchor "middle", transform "translate(0,5) rotate(90 0 0)" ] [ Html.text n ]
        ]