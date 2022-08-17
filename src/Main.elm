module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { hours : Int
    , minutes : Int
    }


init : Model
init =
    { hours = 9
    , minutes = 45
    }


type Msg
    = UpdateHour Int
    | UpdateMinute Int


update msg model =
    case msg of
        UpdateHour newNumber ->
            { model | hours = newNumber }

        UpdateMinute newNumber ->
            { model | minutes = newNumber }


view : Model -> Html Msg
view model =
    Element.layout
        []
    <|
        Element.column
            [ centerX
            , centerY
            , spacing 5
            ]
            [ currentClock [ centerX, Font.size 45 ] model.hours model.minutes
            , currentPeriod [ centerX, Font.size 45 ] model.hours model.minutes
            , countdown [ centerX, Font.size 20 ] model.hours model.minutes
            , hourSlider model.hours
            , minuteSlider model.minutes
            ]


countdown attr hours minutes =
    let
        diff =
            getCountdown ( hours, minutes ) times

        diffString =
            String.fromInt diff

        next =
            getLabel ( hours, minutes + diff + 1 ) <| List.reverse times

        myString =
            if diff < 1 then
                " "

            else
                diffString ++ " minutes until " ++ next
    in
    Element.el attr <| text myString


currentPeriod attr hours minutes =
    Element.el attr <|
        text <|
            getLabel ( hours, minutes ) <|
                List.reverse times


currentClock attr hours minutes =
    let
        amPmHours =
            if hours > 12 then
                hours - 12

            else
                hours

        pm =
            if hours > 11 then
                "pm"

            else
                "am"

        leadingZero =
            if minutes < 10 then
                "0"

            else
                ""

        hourString =
            if amPmHours < 1 then
                "12"

            else
                String.fromInt amPmHours

        minuteString =
            String.fromInt minutes

        myString =
            hourString ++ ":" ++ leadingZero ++ minuteString ++ " " ++ pm
    in
    Element.el attr <| text myString


hourSlider hours =
    Input.slider
        [ Element.height (Element.px 30)
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color red
                , Border.rounded 2
                ]
                Element.none
            )
        , width <| px 500
        ]
        { onChange = \x -> UpdateHour <| round x
        , label =
            Input.labelAbove []
                (text <| String.fromInt hours)
        , min = 0
        , max = 23
        , step = Nothing
        , value = toFloat hours
        , thumb =
            Input.defaultThumb
        }


minuteSlider minutes =
    Input.slider
        [ Element.height (Element.px 30)
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color red
                , Border.rounded 2
                ]
                Element.none
            )
        , width <| px 500
        ]
        { onChange = \x -> UpdateMinute <| round x
        , label =
            Input.labelAbove []
                (text <| String.fromInt minutes)
        , min = 0
        , max = 59
        , step = Nothing
        , value = toFloat minutes
        , thumb =
            Input.defaultThumb
        }


times =
    [ ( ( 0, 0 ), "School hasn't started." )
    , ( ( 8, 44 ), "1st Period" )
    , ( ( 9, 40 ), "Passing Period" )
    , ( ( 9, 44 ), "2nd Period" )
    , ( ( 10, 34 ), "Passing Period" )
    , ( ( 10, 38 ), "3rd Period" )
    , ( ( 11, 28 ), "Passing Period" )
    , ( ( 11, 32 ), "4th Period Lunch A" )
    , ( ( 12, 3 ), "Passing Period" )
    , ( ( 12, 7 ), "4th Period Lunch B" )
    , ( ( 12, 38 ), "Passing Period" )
    , ( ( 12, 42 ), "4th Period Lunch C" )
    , ( ( 13, 13 ), "Passing Period" )
    , ( ( 13, 17 ), "5th Period" )
    , ( ( 14, 7 ), "Passing Period" )
    , ( ( 14, 11 ), "6th Period" )
    , ( ( 15, 1 ), "Passing Period" )
    , ( ( 15, 5 ), "7th Period" )
    , ( ( 15, 55 ), "Schools out." )
    ]


timeToNumber time =
    let
        ( hours, minutes ) =
            time

        timeAsNumber =
            hours * 60 + minutes
    in
    timeAsNumber


getCountdown currentTime schedule =
    case schedule of
        [] ->
            -1

        [ x ] ->
            let
                ( timePair, label ) =
                    x

                myValue =
                    timeToNumber timePair

                currentValue =
                    timeToNumber currentTime
            in
            myValue - currentValue + 1

        x :: xs ->
            let
                ( timePair, label ) =
                    x

                myValue =
                    timeToNumber timePair

                currentValue =
                    timeToNumber currentTime
            in
            if currentValue > myValue then
                getCountdown currentTime xs

            else
                myValue - currentValue + 1


getLabel currentTime schedule =
    case schedule of
        [] ->
            "Oops"

        [ x ] ->
            "School hasn't Started"

        x :: xs ->
            let
                ( timePair, label ) =
                    x

                myValue =
                    timeToNumber timePair

                currentValue =
                    timeToNumber currentTime
            in
            if currentValue > myValue then
                label

            else
                getLabel currentTime xs


red =
    rgb 1 0 0


green =
    rgb 0 1 0


blue =
    rgb 0 0 1


purple =
    rgb 1 0 1
