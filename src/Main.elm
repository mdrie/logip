module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Cldr.Format.DateTime
import Cldr.Locale
import Html exposing (Html, b, div, h1, p, text)
import Http
import Json.Decode
import Task
import Time
import Url exposing (Protocol(..))



-- MODEL


type alias IpAddress =
    { ip : String, lat : Float, lon : Float }


type IpResult
    = Error
    | Success IpAddress


type alias IpReading =
    { result : IpResult
    , timestamp : Time.Posix
    , triggerTime : Time.Posix
    }


type alias IpList =
    List IpReading


type ReadingState
    = None
    | TriggeredAt Time.Posix
    | GotReading Time.Posix IpResult


type alias Model =
    { key : Nav.Key
    , ipList : IpList
    , readingState : ReadingState
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () _ key =
    ( { key = key
      , ipList = []
      , readingState = None
      }
    , Task.perform TimeTick Time.now
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | TimeTick Time.Posix
    | GotIp (Result Http.Error IpAddress)
    | GotTimestamp Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged _ ->
            ( model, Cmd.none )

        TimeTick now ->
            ( { model | readingState = TriggeredAt now }, getIpAddress )

        GotIp result ->
            case model.readingState of
                TriggeredAt triggeredAt ->
                    let
                        ipResult =
                            case result of
                                Result.Ok ipAddr ->
                                    Success ipAddr

                                Result.Err _ ->
                                    Error
                    in
                    ( { model | readingState = GotReading triggeredAt ipResult }, Task.perform GotTimestamp Time.now )

                _ ->
                    ( { model | readingState = None }, Cmd.none )

        GotTimestamp timestamp ->
            case model.readingState of
                GotReading triggeredAt ipResult ->
                    ( { model | readingState = None, ipList = IpReading ipResult timestamp triggeredAt :: model.ipList }, Cmd.none )

                _ ->
                    ( { model | readingState = None }, Cmd.none )



-- addIp : Model -> IpAddress -> Model
-- addIp model ipAddr =
--     { model
--         | ipList =
--             Success
--                 { ip = ipAddr
--                 , timestamp = invTime
--                 , triggerTime = model.lastTriggerTime
--                 }
--                 :: model.ipList
--         , lastTriggerTime = invTime
--     }
-- addTimestamp : Model -> Time.Posix -> Model
-- addTimestamp model time =
--     let
--         newEntry =
--             Success
--                 { ip = ipAddr
--                 , timestamp = time
--                 , triggerTime = model.lastTriggerTime
--                 }
--     in
--     { model | ipList = newEntry :: model.ipList }
-- addError : Model -> Model
-- addError model =
--     { model | ipList = Error :: model.ipList }
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 10000 TimeTick



-- API


getIpAddress : Cmd Msg
getIpAddress =
    Http.get
        { url = "https://api.ip.sb/geoip"
        , expect = Http.expectJson GotIp ipDecoder
        }


ipDecoder : Json.Decode.Decoder IpAddress
ipDecoder =
    Json.Decode.map3 IpAddress
        (Json.Decode.field "ip" Json.Decode.string)
        (Json.Decode.field "latitude" Json.Decode.float)
        (Json.Decode.field "longitude" Json.Decode.float)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Public IP Address Monitor"
    , body =
        [ h1 [] [ text "Public IP Address Monitor" ]
        , b [] [ text "Found IP addresses so far:" ]
        , div []
            (List.map viewReading model.ipList)
        ]
    }


viewReading : IpReading -> Html Msg
viewReading reading =
    p []
        [ text
            (formatTimestamp reading.triggerTime
                ++ " ("
                ++ String.fromInt (Time.posixToMillis reading.timestamp - Time.posixToMillis reading.triggerTime)
                ++ "): "
                ++ (case reading.result of
                        Error ->
                            "Error"

                        Success ipAddress ->
                            ipAddress.ip ++ " (" ++ String.fromFloat ipAddress.lat ++ "," ++ String.fromFloat ipAddress.lon ++ ")"
                   )
            )
        ]


formatTimestamp : Time.Posix -> String
formatTimestamp t =
    Cldr.Format.DateTime.format Cldr.Format.DateTime.medium Cldr.Locale.de Time.utc t ++ "." ++ String.fromInt (Time.toMillis Time.utc t)



-- String.fromInt (Time.toYear Time.utc t) ++ "-"
-- ++ String.fromInt (Time.toMonth Time.utc t) ++ "-"
-- ++ String.fromInt (Time.toDay Time.utc t) ++ " "
-- ++ String.fromInt (Time.toHour Time.utc t) ++ ":"
-- ++ String.fromInt (Time.toMinute Time.utc t) ++ ":"
-- ++ String.fromInt (Time.toSecond Time.utc t) ++ "."
-- ++ String.fromInt (Time.toMillis Time.utc t)
-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
