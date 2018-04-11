module RangeDatePicker
    exposing
        ( DateEvent(..)
        , DatePicker
        , Msg
        , Settings
        , between
        , defaultSettings
        , focusedDate
        , from
        , init
        , initFromDate
        , initFromDates
        , isOpen
        , moreOrLess
        , off
        , pick
        , to
        , update
        , view
        )

{-| A customizable date picker component.


# Tea ☕

@docs Msg, DateEvent, DatePicker
@docs init, initFromDate, initFromDates, update, view, isOpen, focusedDate


# Settings

@docs Settings, defaultSettings, pick, between, moreOrLess, from, to, off

-}

import Date exposing (Date, Day(..), Month, day, month, year)
import DatePicker.Date exposing (..)
import Html exposing (..)
import Html.Attributes as Attrs exposing (defaultValue, href, placeholder, selected, tabindex, type_, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput, onMouseOver, onWithOptions, targetValue)
import Html.Keyed
import Json.Decode as Json
import Task


{-| An opaque type representing messages that are passed inside the DatePicker.
-}
type Msg
    = CurrentDate Date
    | ChangeFirstFocus Date
    | ChangeSecondFocus Date
    | Pick (Maybe Date) (Maybe Date) (Maybe Date) WhichPicker
    | Text String
    | SubmitText
    | Focus
    | Blur
    | MouseDown
    | MouseUp
    | Over (Maybe Date)


{-| The type of date picker settings.
-}
type alias Settings =
    { placeholder : String
    , classNamespace : String
    , inputClassList : List ( String, Bool )
    , inputName : Maybe String
    , inputId : Maybe String
    , inputAttributes : List (Html.Attribute Msg)
    , isDisabled : Date -> Bool
    , parser : String -> Result String Date
    , dateFormatter : Date -> String
    , dayFormatter : Day -> String
    , monthFormatter : Month -> String
    , yearFormatter : Int -> String
    , cellFormatter : String -> Html Msg
    , firstDayOfWeek : Day
    , changeYear : YearRange
    }


type alias Model =
    { open : Bool
    , forceOpen : Bool
    , firstFocused :
        Maybe Date

    -- date currently center-focused by first picker, but not necessarily chosen
    , secondFocused :
        Maybe Date

    -- date currently center-focused by second picker, but not necessarily chosen
    , inputText :
        Maybe String

    -- for user input that hasn't yet been submitted
    , today :
        Date

    -- actual, current day as far as we know
    , hoverDate : Maybe Date

    -- date currently hovered by mouse, but not necessarily chosen
    }


{-| The DatePicker model. Opaque, hence no field docs.
-}
type DatePicker
    = DatePicker Model


type WhichPicker
    = FirstPicker
    | SecondPicker


{-| A record of default settings for the date picker. Extend this if
you want to customize your date picker.

    import DatePicker exposing (defaultSettings)

    DatePicker.init { defaultSettings | placeholder = "Pick a date" }

To disable certain dates:

    import Date exposing (Day(..), dayOfWeek)
    import DatePicker exposing (defaultSettings)

    DatePicker.init { defaultSettings | isDisabled = \d -> dayOfWeek d `List.member` [ Sat, Sun ] }

-}
defaultSettings : Settings
defaultSettings =
    { placeholder = "Please pick a date..."
    , classNamespace = "elm-datepicker--"
    , inputClassList = []
    , inputName = Nothing
    , inputId = Nothing
    , inputAttributes =
        [ Attrs.required False
        ]
    , isDisabled = always False
    , parser = Date.fromString
    , dateFormatter = formatDate
    , dayFormatter = formatDay
    , monthFormatter = formatMonth
    , yearFormatter = toString
    , cellFormatter = formatCell
    , firstDayOfWeek = Sun
    , changeYear = off
    }


yearRangeActive : YearRange -> Bool
yearRangeActive yearRange =
    yearRange /= Off


{-| Select a range of date to display

    DatePicker.init { defaultSettings | changeYear = between 1555 2018 }

-}
between : Int -> Int -> YearRange
between start end =
    if start > end then
        Between end start
    else
        Between start end


{-| Select a symmetric range of date to display

    DatePicker.init { defaultSettings | changeYear = moreOrLess 10 }

-}
moreOrLess : Int -> YearRange
moreOrLess range =
    MoreOrLess range


{-| Select a range from a given year to this year

    DatePicker.init { defaultSettings | changeYear = from 1995 }

-}
from : Int -> YearRange
from year =
    From year


{-| Select a range from this year to a given year

    DatePicker.init { defaultSettings | changeYear = to 2020 }

-}
to : Int -> YearRange
to year =
    To year


{-| Turn off the date range

    DatePicker.init { defaultSettings | changeYear = off }

-}
off : YearRange
off =
    Off


formatCell : String -> Html Msg
formatCell day =
    text day


{-| The default initial state of the Datepicker. You must execute
the returned command (which, for the curious, sets the current date)
for the date picker to behave correctly.

    init =
        let
            ( datePicker, datePickerFx ) =
                DatePicker.init
        in
            { picker = datePicker } ! [ Cmd.map ToDatePicker datePickerfx ]

-}
init : ( DatePicker, Cmd Msg )
init =
    ( DatePicker <|
        { open = False
        , forceOpen = False
        , secondFocused = Just initDate
        , firstFocused = Just initDate
        , inputText = Nothing
        , today = initDate
        , hoverDate = Nothing
        }
    , Task.perform CurrentDate Date.now
    )


{-| Initialize a DatePicker with a given Date

    init date =
        { picker = DatePicker.initFromDate date } ! []

-}
initFromDate : Date -> DatePicker
initFromDate date =
    DatePicker <|
        { open = False
        , forceOpen = False
        , secondFocused = Just date
        , firstFocused = Just date
        , inputText = Nothing
        , today = date
        , hoverDate = Nothing
        }


{-| Initialize a DatePicker with a date for today and Maybe a date picked

    init today date =
        { picker = DatePicker.initFromDates today date } ! []

-}
initFromDates : Date -> Maybe Date -> DatePicker
initFromDates today date =
    DatePicker <|
        { open = False
        , forceOpen = False
        , secondFocused = date
        , firstFocused = date
        , inputText = Nothing
        , today = today
        , hoverDate = Nothing
        }


prepareDates : Date -> Day -> { currentMonth : Date, currentDates : List Date }
prepareDates date firstDayOfWeek =
    let
        start =
            firstOfMonth date |> subDays 6

        end =
            nextMonth date |> addDays 6
    in
        { currentMonth = date
        , currentDates = datesInRange firstDayOfWeek start end
        }


{-| Expose if the datepicker is open
-}
isOpen : DatePicker -> Bool
isOpen (DatePicker model) =
    model.open


{-| Expose the currently focused dates
-}
focusedDate : DatePicker -> ( Maybe Date, Maybe Date )
focusedDate (DatePicker model) =
    (,) model.firstFocused model.secondFocused


{-| A sugaring of `Maybe` to explicitly tell you how to interpret `Changed Nothing`, because `Just Nothing` seems somehow wrong.
Used to represent a request, by the datepicker, to change the selected date.
-}
type DateEvent
    = NoChange
    | Changed (Maybe Date) (Maybe Date)


inDirection : Maybe Date -> Maybe Date -> ( Maybe Date, Maybe Date, Bool )
inDirection firstDate secondDate =
    if
        Maybe.map2 (<) (Maybe.map dateTuple firstDate) (Maybe.map dateTuple secondDate)
            |> Maybe.withDefault True
    then
        ( firstDate, secondDate, True )
    else
        ( secondDate, firstDate, False )


{-| The date picker update function. The third tuple member represents a user action to change the
date.
-}
update : Settings -> Msg -> DatePicker -> ( DatePicker, Cmd Msg, DateEvent )
update settings msg (DatePicker ({ forceOpen, firstFocused, secondFocused } as model)) =
    case msg of
        CurrentDate date ->
            { model
                | firstFocused = Just date
                , secondFocused = Just (nextMonth date)
                , today = date
            }
                ! []

        ChangeFirstFocus date ->
            { model | firstFocused = Just date } ! []

        ChangeSecondFocus date ->
            { model | secondFocused = Just date } ! []

        Pick firstDate secondDate date whichPicker ->
            let
                ( startDate, finishDate, _ ) =
                    inDirection firstDate secondDate

                dateEvent =
                    case startDate of
                        Nothing ->
                            Changed date Nothing

                        _ ->
                            case finishDate of
                                Nothing ->
                                    let
                                        ( earlierDate, laterDate, _ ) =
                                            inDirection startDate date
                                    in
                                        Changed earlierDate laterDate

                                _ ->
                                    Changed date Nothing
            in
                ( DatePicker <|
                    { model
                        | inputText = Nothing
                    }
                , Cmd.none
                , dateEvent
                )

        Text text ->
            { model | inputText = Just text } ! []

        SubmitText ->
            let
                isWhitespace =
                    String.trim >> String.isEmpty

                dateEvent =
                    let
                        text =
                            model.inputText ?> ""
                    in
                        if isWhitespace text then
                            Changed Nothing Nothing
                        else
                            let
                                startDate =
                                    settings.parser (Maybe.withDefault "" (List.head (String.words text)))

                                finishDate =
                                    settings.parser (Maybe.withDefault "" (List.head (List.reverse (String.words text))))
                            in
                                Result.map2
                                    (\firstDate secondDate ->
                                        if settings.isDisabled firstDate then
                                            Changed Nothing Nothing
                                        else
                                            let
                                                ( startDate, finishDate, _ ) =
                                                    inDirection (Just firstDate) (Just secondDate)
                                            in
                                                Changed startDate finishDate
                                    )
                                    startDate
                                    finishDate
                                    |> Result.withDefault NoChange
            in
                ( DatePicker <|
                    { model
                        | inputText =
                            Nothing
                        , firstFocused =
                            case dateEvent of
                                Changed a _ ->
                                    a

                                NoChange ->
                                    model.firstFocused
                        , secondFocused =
                            case dateEvent of
                                Changed _ b ->
                                    b

                                NoChange ->
                                    model.secondFocused
                    }
                , Cmd.none
                , dateEvent
                )

        Focus ->
            { model | open = True, forceOpen = False } ! []

        Blur ->
            { model | open = forceOpen } ! []

        MouseDown ->
            { model | forceOpen = True } ! []

        MouseUp ->
            { model | forceOpen = False } ! []

        Over date ->
            { model | hoverDate = date } ! []


{-| Generate a message that will act as if the user has chosen a certain date,
so you can call `update` on the model yourself.
Note that this is different from just changing the "current chosen" date,
since the picker doesn't actually have internal state for that.
Rather, it will:

  - change the calendar focus

  - replace the input text with the new value

  - close the picker

    update datepickerSettings (pick firstPickedDate secondPickedDate (Just someDate)) datepicker

-}
pick : Maybe Date -> Maybe Date -> Maybe Date -> WhichPicker -> Msg
pick =
    Pick


{-| The date picker view. The Date passed is whatever date it should treat as selected.
-}
view : Maybe Date -> Maybe Date -> Settings -> DatePicker -> Html Msg
view firstDate secondDate settings (DatePicker ({ open } as model)) =
    let
        class =
            mkClass settings

        potentialInputId =
            settings.inputId
                |> Maybe.map Attrs.id
                |> (List.singleton >> List.filterMap identity)

        inputClasses =
            [ ( settings.classNamespace ++ "input", True ) ]
                ++ settings.inputClassList

        inputCommon xs =
            input
                ([ Attrs.classList inputClasses
                 , Attrs.name (settings.inputName ?> "")
                 , type_ "text"
                 , on "change" (Json.succeed SubmitText)
                 , onInput Text
                 , onBlur Blur
                 , onClick Focus
                 , onFocus Focus
                 ]
                    ++ settings.inputAttributes
                    ++ potentialInputId
                    ++ xs
                )
                []

        dateInput =
            inputCommon
                [ placeholder settings.placeholder
                , model.inputText
                    |> Maybe.withDefault
                        ((Maybe.map settings.dateFormatter firstDate
                            |> Maybe.withDefault "..."
                         )
                            ++ " to "
                            ++ (Maybe.map settings.dateFormatter secondDate
                                    |> Maybe.withDefault "..."
                               )
                        )
                    |> value
                ]
    in
        Html.Keyed.node "div"
            [ class "container" ]
            [ ( "dateInput", dateInput )
            , if open then
                ( "doublePicker"
                , div
                    [ class "pickers-container" ]
                    [ datePicker firstDate secondDate settings model model.firstFocused ChangeFirstFocus FirstPicker
                    , datePicker firstDate secondDate settings model model.secondFocused ChangeSecondFocus SecondPicker
                    ]
                )
              else
                ( "text", text "" )
            ]


datePicker : Maybe Date -> Maybe Date -> Settings -> Model -> Maybe Date -> (Date -> Msg) -> WhichPicker -> Html Msg
datePicker firstDate secondDate settings ({ today, hoverDate } as model) focused changeFocusMsg whichPicker =
    let
        currentDate =
            focused ??> firstDate ?> today

        { currentMonth, currentDates } =
            prepareDates currentDate settings.firstDayOfWeek

        class =
            mkClass settings

        classList =
            mkClassList settings

        firstDay =
            settings.firstDayOfWeek

        arrow className message =
            a
                [ class className
                , href "javascript:;"
                , onClick message
                , tabindex -1
                ]
                []

        dow d =
            td [ class "dow" ] [ text <| settings.dayFormatter d ]

        inRange d =
            let
                ( startDate, finishDate, _ ) =
                    inDirection firstDate secondDate
            in
                case startDate of
                    Nothing ->
                        False

                    _ ->
                        case finishDate of
                            Nothing ->
                                let
                                    ( _, _, isLaterThenStart ) =
                                        inDirection startDate d

                                    ( _, _, isEarlierThenHover ) =
                                        inDirection d hoverDate
                                in
                                    if
                                        (isLaterThenStart && isEarlierThenHover)
                                            || (not isLaterThenStart && not isEarlierThenHover)
                                    then
                                        True
                                    else
                                        False

                            _ ->
                                let
                                    ( _, _, isLaterThenStart ) =
                                        inDirection startDate d

                                    ( _, _, isEarlierThenFinish ) =
                                        inDirection d finishDate
                                in
                                    if isLaterThenStart && isEarlierThenFinish then
                                        True
                                    else
                                        False

        picked d =
            (firstDate
                |> Maybe.map
                    (dateTuple >> (==) (dateTuple d))
                |> Maybe.withDefault False
            )
                || (secondDate
                        |> Maybe.map
                            (dateTuple >> (==) (dateTuple d))
                        |> Maybe.withDefault False
                   )

        day d =
            let
                disabled =
                    settings.isDisabled d

                props =
                    if not disabled && (month currentMonth == month d) then
                        [ onClick (Pick firstDate secondDate (Just d) whichPicker)
                        , onMouseOver (Over (Just d))
                        ]
                    else
                        []
            in
                td
                    ([ classList
                        [ ( "day", True )
                        , ( "disabled", disabled )
                        , ( "picked", picked d )
                        , ( "today", dateTuple d == dateTuple today )
                        , ( "other-month", month currentMonth /= month d )
                        , ( "range", inRange (Just d) )
                        ]
                     ]
                        ++ props
                    )
                    [ settings.cellFormatter <| toString <| Date.day d ]

        row days =
            tr [ class "row" ] (List.map day days)

        days =
            List.map row (groupDates currentDates)

        onPicker ev =
            Json.succeed
                >> onWithOptions ev
                    { preventDefault = False
                    , stopPropagation = True
                    }

        onChange handler =
            on "change" <| Json.map handler targetValue

        isCurrentYear selectedYear =
            year currentMonth == selectedYear

        yearOption index selectedYear =
            ( toString index
            , option [ value (toString selectedYear), selected (isCurrentYear selectedYear) ]
                [ text <| toString selectedYear ]
            )

        dropdownYear =
            Html.Keyed.node "select"
                [ onChange (newYear currentDate >> changeFocusMsg), class "year-menu" ]
                (List.indexedMap yearOption
                    (yearRange { currentMonth = currentMonth, today = currentDate } settings.changeYear)
                )
    in
        div
            [ class "picker"
            , onPicker "mousedown" MouseDown
            , onPicker "mouseup" MouseUp
            , tabindex 2
            , onBlur Blur
            ]
            [ div [ class "picker-header" ]
                [ div [ class "prev-container" ]
                    [ arrow "prev" (changeFocusMsg (prevMonth currentDate)) ]
                , div [ class "month-container" ]
                    [ span [ class "month" ]
                        [ text <| settings.monthFormatter <| month currentMonth ]
                    , span [ class "year" ]
                        [ if not (yearRangeActive settings.changeYear) then
                            text <| settings.yearFormatter <| year currentMonth
                          else
                            Html.Keyed.node "span" [] [ ( toString (year currentMonth), dropdownYear ) ]
                        ]
                    ]
                , div [ class "next-container" ]
                    [ arrow "next" (changeFocusMsg (nextMonth currentDate)) ]
                ]
            , table [ class "table" ]
                [ thead [ class "weekdays" ]
                    [ tr []
                        [ dow <| firstDay
                        , dow <| addDows 1 firstDay
                        , dow <| addDows 2 firstDay
                        , dow <| addDows 3 firstDay
                        , dow <| addDows 4 firstDay
                        , dow <| addDows 5 firstDay
                        , dow <| addDows 6 firstDay
                        ]
                    ]
                , tbody [ class "days" ] days
                ]
            ]


{-| Turn a list of dates into a list of date rows with 7 columns per
row representing each day of the week.
-}
groupDates : List Date -> List (List Date)
groupDates dates =
    let
        go i xs racc acc =
            case xs of
                [] ->
                    List.reverse acc

                x :: xs ->
                    if i == 6 then
                        go 0 xs [] (List.reverse (x :: racc) :: acc)
                    else
                        go (i + 1) xs (x :: racc) acc
    in
        go 0 dates [] []


mkClass : Settings -> String -> Html.Attribute msg
mkClass { classNamespace } c =
    Attrs.class (classNamespace ++ c)


mkClassList : Settings -> List ( String, Bool ) -> Html.Attribute msg
mkClassList { classNamespace } cs =
    List.map (\( c, b ) -> ( classNamespace ++ c, b )) cs
        |> Attrs.classList


(!) : Model -> List (Cmd Msg) -> ( DatePicker, Cmd Msg, DateEvent )
(!) m cs =
    ( DatePicker m, Cmd.batch cs, NoChange )


(?>) : Maybe a -> a -> a
(?>) =
    flip Maybe.withDefault


(??>) : Maybe a -> Maybe a -> Maybe a
(??>) first default =
    case first of
        Just val ->
            Just val

        Nothing ->
            default
