module Main exposing (..)

import Browser
import Html
import Html.Attributes
import Http
import List.Extra

import Conflict
import Scatterplot
import ParallelCoordinates
import Tree
import Html.Events
import Model exposing (Msg(..), Model, MainViewType(..), GeoLocationType(..), Filter, init)
import Model exposing (GeoTree)
import Dict
import Html exposing (sub)
import Set

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok newConflicts ->
                    ( { model | conflicts = newConflicts }, Cmd.none)

                Err _ ->
                    ( { model | conflicts = [] }, Cmd.none)

        ChangeMainView newViewType ->
            ( { model | mainViewType = newViewType }, Cmd.none )

        UpdateActiveFilter maybeNewGeoLocationType geoLocation ->
            case maybeNewGeoLocationType of
                Just newGeoLocationType ->
                    let
                        newActiveFilter1 = (newFilter model.activeFilter model.conflicts newGeoLocationType geoLocation)
                        newActiveFilter2 = if (List.isEmpty newActiveFilter1.regions) then { newActiveFilter1 | countries = [] } else newActiveFilter1
                    in
                    ( { model | activeFilter = newActiveFilter2 }, Cmd.none)
                Nothing ->
                    ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
    let
        regions = List.Extra.unique (List.map (.region) model.conflicts)
        conflictView =
            case model.mainViewType of
                ScatterplotView ->
                    Scatterplot.scatterplot (filterConflicts regions model.activeFilter model.conflicts)
                ParallelCoordinatesView year ->
                    let
                        previousDisabled = year==1997
                        nextDisabled = year==2021
                    in
                    Html.div [ Html.Attributes.style "width" "100%", Html.Attributes.style "height" "100%" ]
                        [ Html.button
                            [ Html.Attributes.class "button"
                            , Html.Events.onClick (ChangeMainView ScatterplotView)
                            , Html.Attributes.style "margin-right" "10px"
                            ] [ Html.text "Back" ]
                        , Html.button
                            [ Html.Attributes.class "button"
                            , Html.Events.onClick (ChangeMainView (ParallelCoordinatesView (year - 1)))
                            , Html.Attributes.disabled previousDisabled
                            ] [ Html.text "Previous Year" ]
                        , Html.button
                            [ Html.Attributes.class "button"
                            , Html.Events.onClick (ChangeMainView (ParallelCoordinatesView (year + 1)))
                            , Html.Attributes.disabled nextDisabled
                            ] [ Html.text "Next Year" ]
                        , ParallelCoordinates.parallelCoordinates (filterConflicts regions model.activeFilter model.conflicts) year
                        ]
        eventTypeList = List.Extra.unique (List.map (.event_type) model.conflicts)
    in
    { title = "IRuV-Project"
    , body =
        [ Html.div
            [ Html.Attributes.class "columns", Html.Attributes.style "height" "100%" ]
            [ Html.div [ Html.Attributes.class "column is-1 has-background-info" ]
                []
            , Html.div [ Html.Attributes.class "column is-7", Html.Attributes.style "padding" "30px" ]
                [ Html.h3 [ Html.Attributes.class "title is-3" ] [ Html.text "Africa Conflict 1997-2021" ]
                , conflictView
                ]
            , Html.div
                [ Html.Attributes.class "column is-3"
                , Html.Attributes.style "padding" "30px"
                , Html.Attributes.style "background-color" "#fafafa"
                , Html.Attributes.style "overflow-y" "scroll"
                ]
                [ Html.div []
                    [ Html.h4 [ Html.Attributes.class "title is-4" ] [ Html.text "Geographical Filter:" ]
                    , Tree.renderTree (getTreeData model) model.activeFilter
                    --, Html.ul [] (renderCountryCheckboxes (List.sort (List.Extra.unique (List.map (.country) model.conflicts))) model.activeCountries)
                    ]
                ]
            , Html.div [ Html.Attributes.class "column is-1 has-background-info" ]
                []
            ]
        ]
    }

getTreeData : Model -> GeoTree
getTreeData model =
    let
        regions = List.Extra.unique (List.map (.region) model.conflicts)
        activeRegions = model.activeFilter.regions
        activeCountries = model.activeFilter.countries
        countryNodes = --find for every active region all existing contained countries
            List.map
                (\aR ->
                    (aR
                    , List.Extra.unique
                        (List.map
                            (.country)
                            (List.filter
                                (\c -> c.region==aR)
                                model.conflicts
                            )
                        )
                    )
                )
                activeRegions
    in
    { regions = regions
    , countries = Dict.fromList countryNodes
    }

filterConflicts : List String -> Filter -> List Conflict.Conflict -> List Conflict.Conflict
filterConflicts regions activeFilter conflicts =
    let
        countriesToShow =
            List.Extra.unique
                (List.concat
                    (List.map
                        (\r ->
                            let
                                regionalCountries = Set.fromList (List.map (.country) (List.filter (\c -> c.region==r) conflicts))
                                activeCountries = Set.fromList activeFilter.countries
                                regionalActiveCountries = Set.toList (Set.intersect regionalCountries activeCountries)
                            in
                            if (List.isEmpty regionalActiveCountries) then
                                if (List.member r activeFilter.regions) then
                                    Set.toList regionalCountries
                                else
                                    []
                            else
                                regionalActiveCountries
                        )
                        regions
                    )
                )
    in
    List.filter (\conflict -> (List.member conflict.country countriesToShow)) conflicts

newFilter : Filter -> List Conflict.Conflict -> GeoLocationType -> String -> Filter
newFilter oldFilter conflicts typeOfNewFilter newGeoLocation =
    case typeOfNewFilter of
        Region ->
            if (List.member newGeoLocation oldFilter.regions) then
                let
                    regionalCountries = Set.fromList (List.map (.country) (List.filter (\c -> c.region==newGeoLocation) conflicts))
                    activeCountries = Set.fromList oldFilter.countries
                in
                { oldFilter
                    | regions = (List.Extra.remove newGeoLocation oldFilter.regions)
                    , countries = Set.toList (Set.diff activeCountries regionalCountries)
                }
            else
                { oldFilter | regions = newGeoLocation::oldFilter.regions }
        Country ->
            if (List.member newGeoLocation oldFilter.countries) then
                { oldFilter | countries = List.Extra.remove newGeoLocation oldFilter.countries }
            else
                { oldFilter | countries = newGeoLocation::oldFilter.countries }

