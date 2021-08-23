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
import Model exposing (Msg(..), Model, MainViewType(..), FilterType(..), Filter, init)
import Model exposing (GeoTree)
import Dict

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

        ChangeFilterView newFilterType ->
            ( { model | filterViewType = newFilterType }, Cmd.none )

        UpdateActiveFilter maybeNewFilterType geoLocation ->
            case maybeNewFilterType of
                Just newFilterType ->
                    let
                        newActiveFilter1 = (newFilter model.activeFilter newFilterType geoLocation)
                        newActiveFilter2 = if (List.isEmpty newActiveFilter1.countries) then { newActiveFilter1 | locations = [] } else newActiveFilter1
                        newActiveFilter3 = if (List.isEmpty newActiveFilter2.regions) then { newActiveFilter2 | countries = [], locations = [] } else newActiveFilter2
                    in
                    ( { model | activeFilter = newActiveFilter3, filterViewType = newFilterType }, Cmd.none)
                Nothing ->
                    ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
    let
        conflictView =
            case model.mainViewType of
                ScatterplotView ->
                    Scatterplot.scatterplot (filterConflicts model.activeFilter model.conflicts)
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
                            , Html.Events.onClick (ChangeMainView (ParallelCoordinatesView (year-1)))
                            , Html.Attributes.disabled previousDisabled
                            ] [ Html.text "Previous Year" ]
                        , Html.button
                            [ Html.Attributes.class "button"
                            , Html.Events.onClick (ChangeMainView (ParallelCoordinatesView (year+1)))
                            , Html.Attributes.disabled nextDisabled
                            ] [ Html.text "Next Year" ]
                        , ParallelCoordinates.parallelCoordinates (filterConflicts model.activeFilter model.conflicts) year
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
                [ conflictView
                ]
            , Html.div
                [ Html.Attributes.class "column is-3"
                , Html.Attributes.style "padding" "30px"
                , Html.Attributes.style "background-color" "#fafafa"
                , Html.Attributes.style "overflow-y" "scroll"
                ]
                [ Html.div []
                    [ Html.h4 [ Html.Attributes.class "title is-4" ] [ Html.text "Geographical Filter:" ]
                    , Html.nav
                        [ Html.Attributes.class "breadcrumb has-arrow-separator", Html.Attributes.attribute "aria-label" "breadcrumbs" ]
                        [ Html.ul []
                            [ Html.li []
                                [ Html.a
                                    [ Html.Events.onClick (ChangeFilterView Region) ]
                                    [ Html.text "Filter Region" ]
                                ]
                            , Html.li [ Html.Attributes.class (if List.isEmpty (model.activeFilter.regions) then "is-active" else "") ]
                                [ Html.a
                                    [ Html.Events.onClick (ChangeFilterView Country)]
                                    [ Html.text "Filter Country" ]
                                ]
                            , Html.li [ Html.Attributes.class (if List.isEmpty (model.activeFilter.countries) then "is-active" else "") ]
                                [ Html.a
                                    [ Html.Events.onClick (ChangeFilterView Location) ]
                                    [ Html.text "Filter Location" ]
                                ]
                            ]
                        ]
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
        locationNodes = --find for every active country all existing contained locations
            List.map
                (\aC ->
                    (aC
                    , List.Extra.unique
                        (List.map
                            (.location)
                            (List.filter
                                (\c -> c.country==aC)
                                model.conflicts
                            )
                        )
                    )
                )
                activeCountries


    in
    case model.filterViewType of
        Region ->
            { regions = List.Extra.unique (List.map (.region) model.conflicts)
            , countries = Dict.empty
            , locations = Dict.empty
            }
        Country ->
            { regions = activeRegions
            , countries = Dict.fromList countryNodes
            , locations = Dict.empty
            }
        Location ->
            { regions = activeRegions
            , countries = Dict.fromList countryNodes
            , locations = Dict.fromList locationNodes
            }

filterConflicts : Filter -> List Conflict.Conflict -> List Conflict.Conflict
filterConflicts activeFilter conflicts =
    if (List.isEmpty activeFilter.locations) then
        if (List.isEmpty activeFilter.countries) then
            if (List.isEmpty activeFilter.regions) then
                []
            else
                List.filter (\conflict -> (List.member conflict.region activeFilter.regions)) conflicts
        else
            List.filter (\conflict -> (List.member conflict.country activeFilter.countries)) conflicts
    else
        List.filter (\conflict -> (List.member conflict.location activeFilter.locations)) conflicts

newFilter : Filter -> FilterType -> String -> Filter
newFilter oldFilter typeOfNewFilter newGeoLocation =
    case typeOfNewFilter of
        Region ->
            if (List.member newGeoLocation oldFilter.regions) then
                { oldFilter | regions = List.Extra.remove newGeoLocation oldFilter.regions }
            else
                { oldFilter | regions = newGeoLocation::oldFilter.regions }
        Country ->
            if (List.member newGeoLocation oldFilter.countries) then
                { oldFilter | countries = List.Extra.remove newGeoLocation oldFilter.countries }
            else
                { oldFilter | countries = newGeoLocation::oldFilter.countries }
        Location ->
            if (List.member newGeoLocation oldFilter.locations) then
                { oldFilter | locations = List.Extra.remove newGeoLocation oldFilter.locations }
            else
                { oldFilter | locations = newGeoLocation::oldFilter.locations }

