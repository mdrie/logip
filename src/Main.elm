module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Cldr.Format.DateTime
import Cldr.Locale
import Html exposing (Html, b, button, h1, p, table, td, text, th, tr)
import Html.Attributes exposing (colspan)
import Html.Events exposing (onClick)
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
    | Pause


type alias IpList =
    List IpListEntry


type ReadingState
    = None
    | TriggeredAt Time.Posix
    | GotReading Time.Posix IpResult


type RequestState
    = Paused
    | EveryMillis Float


type alias Model =
    { key : Nav.Key
    , ipList : IpList
    , requestState : RequestState
    , readingState : ReadingState
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () _ key =
    ( { key = key
      , ipList = []
      , requestState = EveryMillis pollInterval
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
    | ToggleState
    | TriggerRequest


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
            case model.readingState of
                None ->
                    ( { model | readingState = TriggeredAt now }, getIpAddress )

                TriggeredAt triggeredAt ->
                    ( handleGap triggeredAt now model, Cmd.none )

                GotReading triggeredAt _ ->
                    -- Should not happen, since GetTimestamp should be quite fast.
                    ( handleGap triggeredAt now model, Cmd.none )

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

                None ->
                    ( model, Cmd.none )

                GotReading _ _ ->
                    -- Should not happen, since GetTimestamp should be quite fast.
                    ( model, Cmd.none )

        GotTimestamp timestamp ->
            case model.readingState of
                GotReading triggeredAt ipResult ->
                    ( { model | readingState = None, ipList = consolidateList (Reading <| IpReading ipResult timestamp triggeredAt) model.ipList }, Cmd.none )

                None ->
                    -- Should not happen. Somehow missed the TimeTick?
                    ( model, Cmd.none )

                TriggeredAt _ ->
                    -- Should not happen. Somehow missed the GotIp?
                    ( { model | readingState = None }, Cmd.none )

        ToggleState ->
            case model.requestState of
                Paused ->
                    ( { model | requestState = EveryMillis pollInterval }, Task.perform TimeTick Time.now )

                _ ->
                    ( { model | requestState = Paused, ipList = Pause :: model.ipList }, Cmd.none )

        TriggerRequest ->
            ( model, Task.perform TimeTick Time.now )


handleGap : Time.Posix -> Time.Posix -> Model -> Model
handleGap triggeredAt now model =
    { model | ipList = insertGap (Time.posixToMillis now - Time.posixToMillis triggeredAt) model.ipList }


insertGap : Int -> IpList -> IpList
insertGap duration list =
    case list of
        (Gap _) :: rest ->
            Gap duration :: rest

        l ->
            Gap duration :: l


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
subscriptions model =
    case model.requestState of
        Paused ->
            Sub.none

        EveryMillis millis ->
            Time.every millis TimeTick



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
        , button [ onClick TriggerRequest ] [ text "Trigger" ]
        , button [ onClick ToggleState ] [ text (buttonText model.requestState) ]
        , p [] [ b [] [ text "Found IP addresses so far:" ] ]
        , table [] (viewHeadRow :: List.map viewEntry model.ipList)
        ]
    }


buttonText : RequestState -> String
buttonText state =
    case state of
        Paused ->
            "Resume"

        EveryMillis _ ->
            "Pause"


viewHeadRow : Html Msg
viewHeadRow =
    tr [] [ th [] [ text "Received at" ], th [] [ text "ms" ], th [] [ text "IP Address" ], th [] [ text "lat" ], th [] [ text "lon" ] ]


viewRow : String -> String -> String -> String -> String -> Html Msg
viewRow timestamp duration ipAddress lat lon =
    tr [] [ td [] [ text timestamp ], td [] [ text duration ], td [] [ text ipAddress ], td [] [ text lat ], td [] [ text lon ] ]


viewSpanRow : String -> Html Msg
viewSpanRow content =
    tr [] [ td [ colspan 5 ] [ text content ] ]


viewEntry : IpListEntry -> Html Msg
viewEntry entry =
    case entry of
        Reading reading ->
            viewReading reading

        Compressed times ->
            viewSpanRow <| "... repeated " ++ String.fromInt times ++ " times ..."

        Gap millis ->
            viewSpanRow <| "... here is a gap of " ++ String.fromInt millis ++ "ms ..."

        Pause ->
            viewSpanRow <| "--- Pause ---"


viewReading : IpReading -> Html Msg
viewReading reading =
    let
        time =
            formatTimestamp reading.triggerTime

        duration =
            String.fromInt (Time.posixToMillis reading.timestamp - Time.posixToMillis reading.triggerTime)

        ( ip, lat, lon ) =
            case reading.result of
                Error ->
                    ( "Error", "", "" )

                Success ipAddress ->
                    ( ipAddress.ip, String.fromFloat ipAddress.lat, String.fromFloat ipAddress.lon )
    in
    viewRow time duration ip lat lon


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
