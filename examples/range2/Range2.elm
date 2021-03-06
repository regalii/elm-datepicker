module Range2 exposing (main)

import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import Html exposing (Html, div, h1, text)
import RangeDatePicker as DatePicker exposing (DateEvent(..), defaultSettings)


type Msg
    = ToDatePicker DatePicker.Msg


type alias Model =
    { startDate : Maybe Date
    , finishDate : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


settings : DatePicker.Settings
settings =
    defaultSettings


init : ( Model, Cmd Msg )
init =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    { startDate = Nothing
    , finishDate = Nothing
    , datePicker = datePicker
    }
        ! [ Cmd.map ToDatePicker datePickerFx ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ startDate, finishDate, datePicker } as model) =
    case msg of
        ToDatePicker msg ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update settings msg datePicker
            in
            case dateEvent of
                Changed startDate finishDate ->
                    { model
                        | startDate = startDate
                        , finishDate = finishDate
                        , datePicker = newDatePicker
                    }
                        ! [ Cmd.map ToDatePicker datePickerFx ]

                _ ->
                    { model
                        | datePicker = newDatePicker
                    }
                        ! [ Cmd.map ToDatePicker datePickerFx ]


view : Model -> Html Msg
view ({ startDate, finishDate, datePicker } as model) =
    div []
        [ case startDate of
            Nothing ->
                h1 [] [ text "Pick a date" ]

            Just date ->
                h1 [] [ text <| formatDate date ]
        , case finishDate of
            Nothing ->
                h1 [] [ text "Pick a date" ]

            Just date ->
                h1 [] [ text <| formatDate date ]
        , DatePicker.view startDate finishDate settings datePicker
            |> Html.map ToDatePicker
        ]


formatDate : Date -> String
formatDate d =
    toString (month d) ++ " " ++ toString (day d) ++ ", " ++ toString (year d)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
