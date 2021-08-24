module Tree exposing (..)

import Html
import Html.Events
import Html.Attributes
import Svg exposing (Svg, line, circle, text_, g)
import Svg.Attributes exposing (x1, y1, x2, y2, stroke, textAnchor, transform)
import TypedSvg exposing (style)
import TreeDiagram exposing (node, Tree, defaultTreeLayout, leftToRight)
import TreeDiagram.Svg exposing (draw)

import Model exposing (Msg(..), GeoLocationType(..), GeoTree)
import Dict exposing (Dict)
import Svg exposing (rect)
import TypedSvg.Core
import Model exposing (Filter)

renderTree : GeoTree -> Filter -> Svg Msg
renderTree geoTree activeFilter =
    let
        tree = buildTree geoTree
    in
    draw { defaultTreeLayout | orientation = leftToRight } (drawNode activeFilter) drawLine tree

buildTree : GeoTree -> Tree (Maybe GeoLocationType, String)
buildTree geoTree =
    let
        regions = geoTree.regions
        countries = geoTree.countries
        countryNodes =
            Dict.map
                (\key countryNames ->
                    List.map
                        (\countryName ->
                            node
                                (Just Country, countryName)
                                []
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
        [ toString x1 (0+(nodeWidth/2)), toString y1 0, (toString x2 (targetX - (nodeWidth/2))), toString y2 targetY, stroke "black" ]
        []

nodeWidth = 80

drawNode : Filter -> (Maybe GeoLocationType, String) -> Svg Msg
drawNode activeFilter (maybeFilterType, name) =
    let
        widthString = String.concat [ (String.fromInt nodeWidth), "px" ]
        offsetString = String.concat [ (String.fromInt (floor (negate (nodeWidth/2)))), "px" ]
        nodeName =
            case maybeFilterType of
                Just Region -> (shortenRegionName name)
                _ -> (checkAndShortenName name)
        nodeClass =
            case maybeFilterType of
                Just _ -> "treeNodeBox"
                Nothing -> "rootNodeBox"
        isActiveClass =
            case maybeFilterType of
                Just Region ->
                    if (List.member name activeFilter.regions) then "activeNodeBox" else ""
                Just Country ->
                    if (List.member name activeFilter.countries) then "activeNodeBox" else ""
                Nothing -> ""
    in
    g
        [ Svg.Attributes.class (String.concat [nodeClass," ",isActiveClass]) ]
        [ text_ [ textAnchor "middle", transform "translate(0,5) rotate(0 0 0)" ] [ Html.text nodeName ]
        , rect
            [ Svg.Attributes.height "20px"
            , Svg.Attributes.width widthString
            , Svg.Attributes.x offsetString
            , Svg.Attributes.y "-10px"
            , Html.Events.onClick (UpdateActiveFilter maybeFilterType name)
            , Html.Events.onMouseEnter (ShowGeoLocationName name)
            , Html.Events.onMouseLeave (ShowGeoLocationName " ")
            ] []
        ]

shortenRegionName : String -> String
shortenRegionName name =
    case name of
        "Western Africa" -> "West"
        "Eastern Africa" -> "East"
        "Southern Africa" -> "South"
        "Northern Africa" -> "North"
        "Middle Africa" -> "Middle"
        _ -> "undefined"

checkAndShortenName : String -> String
checkAndShortenName name =
    let
        shortName =
            String.concat
                (List.map
                    (\s ->
                        String.append (String.left 1 s) ". "
                    )
                    (String.split " " name)
                )
    in
    if (String.length name)>9
    then shortName
    else name