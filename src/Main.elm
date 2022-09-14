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



-- CONSTANTS


pollInterval : number
pollInterval =
    10000



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


type IpListEntry
    = Reading IpReading
    | Compressed Int
    | Gap Int


type alias IpList =
    List IpListEntry


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
                    ( { model | readingState = None, ipList = consolidateList (Reading (IpReading ipResult timestamp triggeredAt)) model.ipList }, Cmd.none )

                _ ->
                    ( { model | readingState = None }, Cmd.none )


consolidateList : IpListEntry -> IpList -> IpList
consolidateList newEntry list =
    case newEntry of
        Reading first ->
            case list of
                (Reading second) :: (Compressed count) :: rest ->
                    if readingsEqual first second then
                        Reading first :: Compressed (count + 1) :: rest

                    else
                        Reading first :: list

                (Reading second) :: (Reading third) :: rest ->
                    if readingsEqual first second && readingsEqual first third then
                        Reading first :: Compressed 1 :: Reading third :: rest

                    else
                        Reading first :: list

                _ ->
                    Reading first :: list

        _ ->
            newEntry :: list


readingsEqual : IpReading -> IpReading -> Bool
readingsEqual first second =
    case ( first.result, second.result ) of
        ( Success f, Success s ) ->
            (f.ip == s.ip) && f.lat == s.lat && f.lon == s.lon

        ( Error, Error ) ->
            True

        _ ->
            False



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every pollInterval TimeTick



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
            (List.map viewEntry model.ipList)
        ]
    }


viewEntry : IpListEntry -> Html Msg
viewEntry entry =
    case entry of
        Reading reading ->
            viewReading reading

        Compressed times ->
            p [] [ text ("... repeated " ++ String.fromInt times ++ " times ...") ]

        Gap millis ->
            p [] [ text ("... here is a gap of " ++ String.fromInt millis ++ "ms ...") ]


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
