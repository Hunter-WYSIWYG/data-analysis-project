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
import Model exposing (Msg(..), Model, MainViewType(..), FilterViewType(..), init)
import Model exposing (GeoTree)

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

        UpdateSelectedCountries country ->
            ( { model | activeCountries = (newCountries model.activeCountries country) }, Cmd.none)

        ChangeMainView newViewType ->
            ( { model | mainViewType = newViewType }, Cmd.none )

        ChangeFilterView newFilterView ->
            ( { model | filterViewType = newFilterView }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
    let
        conflictView =
            case model.mainViewType of
                ScatterplotView ->
                    Scatterplot.scatterplot (filterConflictsByCountries model.conflicts model.activeCountries)
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
                        , ParallelCoordinates.parallelCoordinates (filterConflictsByCountries model.conflicts model.activeCountries) year
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
            , Html.div [ Html.Attributes.class "column is-3", Html.Attributes.style "padding" "30px", Html.Attributes.style "background-color" "#fafafa" ]
                [ Html.div 
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "top" "50%"
                    , Html.Attributes.style "transform" "translate(0, -50%)"
                    ]
                    [ Html.h4 [ Html.Attributes.class "title is-4" ] [ Html.text "Geographical Filter:" ]
                    , Html.nav
                        [ Html.Attributes.class "breadcrumb has-arrow-separator", Html.Attributes.attribute "aria-label" "breadcrumbs" ]
                        [ Html.ul []
                            [ Html.li []
                                [ Html.a
                                    [ Html.Events.onClick (ChangeFilterView Region) ]
                                    [ Html.text "Filter Region" ]
                                ]
                            , Html.li []
                                [ Html.a
                                    [ Html.Events.onClick (ChangeFilterView Country) ]
                                    [ Html.text "Filter Country" ]
                                ]
                            , Html.li []
                                [ Html.a
                                    [ Html.Events.onClick (ChangeFilterView Location) ]
                                    [ Html.text "Filter Location" ]
                                ]
                            ]
                        ]
                    , Tree.renderTree (getTreeData model)
                    , Html.ul [] (renderCountryCheckboxes (List.sort (List.Extra.unique (List.map (.country) model.conflicts))) model.activeCountries)
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
            , countries = []
            , locations = []
            }
        Country ->
            { regions = activeRegions
            , countries = countryNodes
            , locations = []
            }
        Location ->
            { regions = activeRegions
            , countries = countryNodes
            , locations = locationNodes
            }

filterConflictsByCountries : List Conflict.Conflict -> List String -> List Conflict.Conflict
filterConflictsByCountries conflicts countries =
    List.filter (\conflict -> (List.member conflict.country countries)) conflicts

newCountries : List String -> String -> List String
newCountries oldCountries newCountry =
    if (List.member newCountry oldCountries) then
        List.Extra.remove newCountry oldCountries
    else
        newCountry::oldCountries

renderCountryCheckboxes : List String -> List String -> List (Html.Html Msg)
renderCountryCheckboxes countries activeCountries =
    List.map
        (\c ->
            let
                isActive = List.member c activeCountries
            in
            Html.li []
                [ Html.label [ Html.Attributes.class "checkbox", Html.Events.onClick (UpdateSelectedCountries c) ]
                    [ Html.input
                        [ Html.Attributes.type_ "checkbox"
                        , Html.Attributes.style "margin-right" "5px"
                        , Html.Attributes.checked isActive
                        ] []
                    ]
                , Html.text c
                ]
        )
        countries

