module Main exposing (..)

import Browser
import Html
import Html.Attributes
import Http
import List.Extra

import Conflict
import Scatterplot
import ParallelCoordinates
import Html.Events
import Model exposing (Msg(..), Model, ViewType(..), init)

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

        ChangeView newViewType ->
            ( { model | viewType = newViewType }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
    let
        conflictView =
            case model.viewType of
                ScatterplotView ->
                    Scatterplot.scatterplot model.conflicts (filterConflictsByCountries model.conflicts model.activeCountries)
                ParallelCoordinatesView year ->
                    Html.div [ Html.Attributes.style "width" "100%", Html.Attributes.style "height" "100%" ]
                        [ Html.button [ Html.Attributes.class "button", Html.Events.onClick (ChangeView ScatterplotView) ] [ Html.text "Back" ]
                        , ParallelCoordinates.parallelCoordinates (filterConflictsByCountries model.conflicts model.activeCountries) year
                        ]
                TreeView ->
                    Html.div [] []
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
                [ Html.div []
                    [ Html.ul []
                        (renderCountryCheckboxes (List.sort (List.Extra.unique (List.map (.country) model.conflicts))))
                    ]
                ]
            , Html.div [ Html.Attributes.class "column is-1 has-background-info" ]
                []
            ]
        ]
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

renderCountryCheckboxes : List String -> List (Html.Html Msg)
renderCountryCheckboxes countries =
    List.map
        (\c ->
            Html.li []
                [ Html.label [ Html.Attributes.class "checkbox", Html.Events.onClick (UpdateSelectedCountries c) ]
                    [ Html.input [ Html.Attributes.type_ "checkbox", Html.Attributes.style "margin-right" "5px" ] []
                    ]
                , Html.text c
                ]
        )
        countries