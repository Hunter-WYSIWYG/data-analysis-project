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
    { title = "IRuV-Project"
    , body =
        [ Html.div
            [ Html.Attributes.class "columns", Html.Attributes.style "height" "100%" ]
            [ Html.div [ Html.Attributes.class "column is-1 has-background-info" ]
                []
            , Html.div [ Html.Attributes.class "column is-7", Html.Attributes.style "padding" "30px" ]
                [ Scatterplot.scatterplot model.conflicts (Scatterplot.filterConflictForCountries model.conflicts model.activeCountries)
                ]
            , Html.div [ Html.Attributes.class "column is-3", Html.Attributes.style "padding" "30px", Html.Attributes.style "background-color" "#fafafa" ]
                [ Html.div []
                    [ Html.ul []
                        (Scatterplot.renderCountryCheckboxes (List.sort (List.Extra.unique (List.map (.country) model.conflicts))))
                    ]
                ]
            , Html.div [ Html.Attributes.class "column is-1 has-background-info" ]
                []
            ]
        ]
    }

newCountries : List String -> String -> List String
newCountries oldCountries newCountry =
    if (List.member newCountry oldCountries) then
        List.Extra.remove newCountry oldCountries
    else
        newCountry::oldCountries