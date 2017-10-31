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
        , pickFinish
        , pickStart
        , to
        , update
        , view
        )

{-| A customizable date picker component.


# Tea ☕

@docs Msg, DateEvent, DatePicker
@docs init, initFromDate, initFromDates, update, view, isOpen, focusedDate


# Settings

@docs Settings, defaultSettings, pickStart, pickFinish, between, moreOrLess, from, to, off

-}

import Date exposing (Date, Day(..), Month, day, month, year)
import DatePicker.Date exposing (..)
import Html exposing (..)
import Html.Attributes as Attrs exposing (defaultValue, href, placeholder, selected, tabindex, type_, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput, onMouseLeave, onMouseOver, onWithOptions, targetValue)
import Html.Keyed
import Json.Decode as Json
import Task


{-| An opaque type representing messages that are passed inside the DatePicker.
-}
type Msg
    = CurrentDate Date
    | ChangeFocus Date
    | PickStart (Maybe Date)
    | PickFinish (Maybe Date)
    | StartText String
    | FinishText String
    | SubmitText (Maybe String) (Maybe Date -> DateEvent)
    | Focus (Maybe Date -> Msg)
    | Blur
    | MouseDown
    | MouseUp
    | Over (Maybe Date)
    | MouseLeave
    | ErrorClick


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
    , focused :
        Maybe Date

    -- date currently center-focused by picker, but not necessarily chosen
    , inputStartText :
        Maybe String

    -- for user start input that hasn't yet been submitted
    , inputFinishText :
        Maybe String

    -- for user finsh input that hasn't yet been submitted
    , today :
        Date

    -- actual, current day as far as we know
    , hoverDate : Maybe Date

    -- mouse hovered dates for drawing background color of range
    , pickEvent : Maybe Date -> Msg

    -- PickStart or PickFinish depends on who's initiator of datapicker opening
    }


{-| The DatePicker model. Opaque, hence no field docs.
-}
type DatePicker
    = DatePicker Model


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
        , focused = Just initDate
        , inputStartText = Nothing
        , inputFinishText = Nothing
        , today = initDate
        , hoverDate = Nothing
        , pickEvent = PickStart
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
        , focused = Just date
        , inputStartText = Nothing
        , inputFinishText = Nothing
        , today = date
        , hoverDate = Nothing
        , pickEvent = PickStart
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
        , focused = date
        , inputStartText = Nothing
        , inputFinishText = Nothing
        , today = today
        , hoverDate = Nothing
        , pickEvent = PickStart
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


{-| Expose the currently focused date
-}
focusedDate : DatePicker -> Maybe Date
focusedDate (DatePicker model) =
    model.focused


inputText : Maybe Date -> Maybe Date -> Settings -> Maybe String
inputText startDate finishDate settings =
    Maybe.map2 (++) (Maybe.map settings.dateFormatter finishDate) (Maybe.map ((++) " - ") (Maybe.map settings.dateFormatter startDate))


{-| A sugaring of `Maybe` to explicitly tell you how to interpret `Changed Nothing`, because `Just Nothing` seems somehow wrong.
Used to represent a request, by the datepicker, to change the selected date.
-}
type DateEvent
    = NoChange
    | ChangedStart (Maybe Date)
    | ChangedFinish (Maybe Date)


{-| The date picker update function. The third tuple member represents a user action to change the
date.
-}
update : Settings -> Msg -> DatePicker -> ( DatePicker, Cmd Msg, DateEvent )
update settings msg (DatePicker ({ forceOpen, focused, hoverDate } as model)) =
    case msg of
        CurrentDate date ->
            { model | focused = Just date, today = date } ! []

        ChangeFocus date ->
            { model | focused = Just date } ! []

        MouseLeave ->
            { model | open = False } ! []

        Over date ->
            { model
                | hoverDate = date
            }
                ! []

        PickStart date ->
            ( DatePicker <|
                { model
                    | open = True
                    , inputStartText = Nothing
                    , focused = Nothing
                    , pickEvent = PickFinish
                }
            , Cmd.none
            , ChangedStart date
            )

        PickFinish date ->
            ( DatePicker <|
                { model
                    | open = True
                    , inputStartText = Nothing
                    , focused = Nothing
                    , pickEvent = PickStart
                }
            , Cmd.none
            , ChangedFinish date
            )

        StartText text ->
            { model | inputStartText = Just text } ! []

        FinishText text ->
            { model | inputFinishText = Just text } ! []

        SubmitText inputText dateEventChanged ->
            let
                isWhitespace =
                    String.trim >> String.isEmpty

                dateEvent =
                    let
                        text =
                            inputText ?> ""
                    in
                    if isWhitespace text then
                        dateEventChanged Nothing
                    else
                        text
                            |> settings.parser
                            |> Result.map
                                (dateEventChanged
                                    << (\date ->
                                            if settings.isDisabled date then
                                                Nothing
                                            else
                                                Just date
                                       )
                                )
                            |> Result.withDefault NoChange
            in
            ( DatePicker <|
                { model
                    | inputStartText =
                        case dateEvent of
                            NoChange ->
                                model.inputStartText

                            _ ->
                                Nothing
                    , inputFinishText =
                        case dateEvent of
                            NoChange ->
                                model.inputFinishText

                            _ ->
                                Nothing
                    , focused =
                        case dateEvent of
                            NoChange ->
                                model.focused

                            _ ->
                                Nothing
                }
            , Cmd.none
            , dateEvent
            )

        Focus pickEvent ->
            { model | open = True, forceOpen = False, pickEvent = pickEvent } ! []

        Blur ->
            { model | open = forceOpen } ! []

        MouseDown ->
            { model | forceOpen = True } ! []

        MouseUp ->
            { model | forceOpen = False } ! []

        ErrorClick ->
            model ! []


{-| Generate a message that will act as if the user has chosen a certain date,
so you can call `update` on the model yourself.
Note that this is different from just changing the "current chosen" date,
since the picker doesn't actually have internal state for that.
Rather, it will:

  - change the calendar focus

  - replace the input text with the new value

  - close the picker for second pick

    update datepickerSettings (pick (Just someDate)) datepicker

-}
pickStart : Maybe Date -> Msg
pickStart =
    PickStart


{-| Like under
-}
pickFinish : Maybe Date -> Msg
pickFinish =
    PickFinish


{-| The date picker view. The Date passed is whatever date it should treat as selected.
-}
view : Maybe Date -> Maybe Date -> Settings -> DatePicker -> Html Msg
view startDate finishDate settings (DatePicker ({ open, pickEvent } as model)) =
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

        inputCommon inputText textEvent dateEvent pickEvent xs =
            input
                ([ Attrs.classList inputClasses
                 , Attrs.name (settings.inputName ?> "")
                 , type_ "text"
                 , on "change" (Json.succeed (SubmitText inputText dateEvent))
                 , onInput textEvent
                 , onBlur Blur
                 , onClick (Focus pickEvent)
                 , onFocus (Focus pickEvent)
                 ]
                    ++ settings.inputAttributes
                    ++ potentialInputId
                    ++ xs
                )
                []

        dateInput inputText textEvent dateEvent date pickEvent =
            inputCommon
                inputText
                textEvent
                dateEvent
                pickEvent
                [ placeholder settings.placeholder
                , inputText
                    |> Maybe.withDefault
                        (Maybe.map settings.dateFormatter date
                            |> Maybe.withDefault ""
                        )
                    |> defaultValue
                ]
    in
    Html.Keyed.node "div"
        [ class "container" ]
        [ ( "dateInput", dateInput model.inputStartText StartText ChangedStart startDate PickStart )
        , ( "text", text " - " )
        , ( "dateInput", dateInput model.inputFinishText FinishText ChangedFinish finishDate PickFinish )
        , if open then
            ( "datePicker", datePicker startDate finishDate settings pickEvent model )
          else
            ( "text", text "" )
        ]


isLater : Maybe Date -> Date -> Bool
isLater maybeDate date =
    maybeDate
        |> Maybe.map
            (dateTuple >> (>) (dateTuple date))
        |> Maybe.withDefault False


isEqual : Maybe Date -> Date -> Bool
isEqual maybeDate date =
    maybeDate
        |> Maybe.map
            (dateTuple >> (==) (dateTuple date))
        |> Maybe.withDefault False


datePicker : Maybe Date -> Maybe Date -> Settings -> (Maybe Date -> Msg) -> Model -> Html Msg
datePicker startDate finishDate settings pickEvent ({ focused, today, hoverDate } as model) =
    let
        currentDate =
            focused ??> startDate ?> today

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
            case startDate of
                Nothing ->
                    False

                _ ->
                    case finishDate of
                        Nothing ->
                            if isLater startDate d && not (isLater hoverDate d) then
                                True
                            else
                                False

                        _ ->
                            if isLater startDate d && not (isLater finishDate d) then
                                True
                            else
                                False

        picked d =
            isEqual finishDate d || isEqual startDate d

        day d =
            let
                disabled =
                    settings.isDisabled d

                props =
                    if not disabled then
                        [ onClick (pickEvent (Just d))
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
                    , ( "today", dateTuple d == dateTuple currentDate )
                    , ( "other-month", month currentMonth /= month d )
                    , ( "range", inRange d )
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
                [ onChange (newYear currentDate >> ChangeFocus), class "year-menu" ]
                (List.indexedMap yearOption
                    (yearRange { focused = currentDate, currentMonth = currentMonth } settings.changeYear)
                )
    in
    div
        [ class "picker"
        , onPicker "mousedown" MouseDown
        , onPicker "mouseup" MouseUp
        , onMouseLeave MouseLeave
        ]
        [ div [ class "picker-header" ]
            [ div [ class "prev-container" ]
                [ arrow "prev" (ChangeFocus (prevMonth currentDate)) ]
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
                [ arrow "next" (ChangeFocus (nextMonth currentDate)) ]
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
