module Home exposing (Model, Msg, init, subscriptions, update, view)

import Element exposing (Element, column, el, fill, fillPortion, link, padding, paragraph, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as J
import List
import MopidyRPC exposing (Error, librarySearchTrack, method, playbackGetState, playbackGetTimePosition, request, trackListSetConsume, tracklistAdd, tracklistGetTracks)
import MopidyRPC.Data as Data exposing (PlaybackState(..))
import Session
import Time
import Utils exposing (brighter, button, darker, good, icon, lengthToTime, lightgrey, onEnter, primary, ruled, smallButton, smallFont, smallIcon, trackToElement, warn)


type alias Model =
    { session : Session.Data
    , searchContent : String
    , playing : Data.PlaybackState
    , position : Maybe Int
    , queue : List Data.Track
    , searchResults : List Data.Track
    , message : String
    }


type Msg
    = None
    | Tick
    | GotPlaybackState (Result Error Data.PlaybackState)
    | GotTrackList (Result Error (List Data.Track))
    | GotPosition (Result Error (Maybe Int))
    | GotSearchResult (Result Error (List Data.Track))
    | GotTrackAdd (Result Error (List Data.TlTrack))
    | GotSkip (Result Http.Error (Result Bool Int))
    | PlayPause
    | Skip
    | UpdateSearch String
    | DoTrackAdd String
    | DoTrackAddNext String
    | EnterWasPressed
    | GetTrackList


init : Session.Data -> ( Model, Cmd Msg )
init data =
    ( { session = data
      , searchContent = ""
      , playing = Stopped
      , position = Nothing
      , queue = []
      , searchResults = []
      , message = ""
      }
    , Cmd.batch [ tickCmd, trackListSetConsume True (always None) |> request ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Tick ->
            ( model, tickCmd )

        GotPlaybackState result ->
            case result of
                Ok status ->
                    ( { model | playing = status }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotTrackList result ->
            case result of
                Ok tracks ->
                    ( { model | queue = tracks }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotPosition result ->
            case result of
                Ok position ->
                    ( { model | position = position }, Cmd.none )

                Err _ ->
                    ( { model | position = Nothing }, Cmd.none )

        PlayPause ->
            case model.playing of
                Data.Playing ->
                    ( { model | message = "", playing = Data.Paused }, pause )

                Data.Paused ->
                    ( { model | message = "", playing = Data.Playing }, play )

                Data.Stopped ->
                    ( { model | message = "" }, play )

        Skip ->
            ( model, voteSkip model )

        GotSkip result ->
            case result of
                Ok res ->
                    case res of
                        Ok needs ->
                            ( { model | message = "Need " ++ String.fromInt needs ++ " more vote(s) to skip" }, Cmd.none )

                        Err _ ->
                            ( { model | message = "" }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        UpdateSearch content ->
            case content of
                "" ->
                    ( { model | searchResults = [], searchContent = content }, Cmd.none )

                _ ->
                    ( { model | searchContent = content }, Cmd.none )

        EnterWasPressed ->
            case model.searchContent of
                "" ->
                    ( { model | message = "" }, Cmd.none )

                _ ->
                    ( { model | message = "Loading..." }, librarySearchTrack model.searchContent GotSearchResult |> request )

        GetTrackList ->
            ( model, Cmd.batch [ getTrackList, getPosition ] )

        GotSearchResult result ->
            case result of
                Ok tracks ->
                    ( { model | message = "", searchResults = tracks }, Cmd.none )

                Err _ ->
                    ( { model | message = "Error occured" }, Cmd.none )

        DoTrackAdd uri ->
            ( { model | message = "" }, addTrack uri )

        DoTrackAddNext uri ->
            ( { model | message = "" }, addTrackNext uri )

        GotTrackAdd result ->
            case ( result, model.playing ) of
                ( Ok _, Stopped ) ->
                    ( model, play )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 <| always Tick


view : Model -> ( String, List (Element Msg) )
view model =
    ( "Moparty Home"
    , [ buttonRow model
      , messageRow model
      , searchRow model
      , resultsRow model
      ]
    )


messageRow : Model -> Element Msg
messageRow model =
    case model.message of
        "" ->
            Element.none

        msg ->
            text msg


buttonRow : Model -> Element Msg
buttonRow model =
    wrappedRow [ width fill ]
        [ row [ width (fillPortion 1), spacing 8 ]
            [ case model.playing of
                Data.Playing ->
                    button warn PlayPause <| icon "pause" "Pause"

                Data.Paused ->
                    button good PlayPause <| icon "play" "Play"

                Data.Stopped ->
                    button (darker <| darker good) PlayPause <| icon "play" "Play"
            , button primary Skip <| icon "step-forward" "Skip current song"
            ]
        , column [ spacing 8, Border.rounded 3, padding 16, width (fillPortion 1), Background.color lightgrey ]
            (case model.queue of
                [] ->
                    [ text "Nothing in queue" ]

                _ ->
                    upNext model
            )
        ]


upNextElem model track =
    let
        played =
            Maybe.withDefault "" (Maybe.map (\x -> lengthToTime x ++ " / ") model.position)
    in
    paragraph []
        [ text track.name
        , el smallFont <| text <| " by " ++ (String.join " & " <| List.map .name track.artists)
        , el smallFont <| text <| " (" ++ played ++ Maybe.withDefault "?:??" (Maybe.map lengthToTime track.length) ++ ")"
        ]


upNext : Model -> List (Element Msg)
upNext model =
    case model.queue of
        [] ->
            []

        first :: xs ->
            upNextElem model first
                :: (case xs of
                        [] ->
                            []

                        second :: ys ->
                            row [ spacing 8 ]
                                [ smallIcon "arrow-right" "Up next"
                                , paragraph []
                                    [ el smallFont <| text (second.name ++ " by " ++ (String.join " & " <| List.map .name second.artists))
                                    , el smallFont <| text <| "(" ++ Maybe.withDefault "?:??" (Maybe.map lengthToTime second.length) ++ ")"
                                    ]
                                ]
                                :: (case ys of
                                        [] ->
                                            []

                                        more ->
                                            link [ Font.underline ]
                                                { url = "queue/"
                                                , label =
                                                    row [ spacing 8 ]
                                                        [ smallIcon "plus" "Plus"
                                                        , el smallFont <| text ((List.length more |> String.fromInt) ++ " more songs")
                                                        ]
                                                }
                                                :: []
                                   )
                   )


searchRow model =
    row [ width fill, spacing 16 ]
        [ Input.text [ Input.focusedOnLoad, onEnter EnterWasPressed ]
            { label = Input.labelHidden "Search"
            , onChange = UpdateSearch
            , placeholder = Just <| Input.placeholder [] (text "Search")
            , text = model.searchContent
            }
        , button good EnterWasPressed <| icon "search" "Search"
        ]


wasAdded : Model -> Data.Track -> Bool
wasAdded model track =
    List.filter (\x -> x.uri == track.uri) model.queue
        |> List.length
        |> (/=) 0


resultsRow : Model -> Element Msg
resultsRow model =
    ruled [ width fill ]
        { data = model.searchResults
        , viewf =
            \track ->
                if wasAdded model track then
                    trackToElement [ el [ Border.rounded 3, padding 16, Background.color <| brighter good ] (icon "check" "Added") ] track

                else
                    trackToElement
                        [ button good (DoTrackAdd track.uri) <| icon "plus" "Add to end"
                        , smallButton primary (DoTrackAddNext track.uri) <| icon "arrow-up" "Add next"
                        ]
                        track
        }


tickCmd =
    Cmd.batch [ getStatus, getPosition, getTrackList ]


play : Cmd Msg
play =
    request { toMsg = always None, body = method "core.playback.play", decoder = Decode.null () }


pause : Cmd Msg
pause =
    request { toMsg = always None, body = method "core.playback.pause", decoder = Decode.null () }


getStatus : Cmd Msg
getStatus =
    playbackGetState GotPlaybackState
        |> request


getTrackList : Cmd Msg
getTrackList =
    tracklistGetTracks GotTrackList
        |> request


getPosition : Cmd Msg
getPosition =
    playbackGetTimePosition GotPosition
        |> request


addTrack : String -> Cmd Msg
addTrack uri =
    tracklistAdd [ uri ] Nothing GotTrackAdd
        |> request


addTrackNext : String -> Cmd Msg
addTrackNext uri =
    tracklistAdd [ uri ] (Just 1) GotTrackAdd
        |> request


voteSkip : Model -> Cmd Msg
voteSkip model =
    case model.session.clientId of
        Just clientId ->
            Http.post
                { url = "/" ++ model.session.urlPrefix ++ "/api/skip"
                , body = J.object [ ( "client_id", J.int clientId ) ] |> Http.jsonBody
                , expect =
                    Http.expectJson GotSkip <|
                        Decode.oneOf
                            [ Decode.map Err <| Decode.field "success" Decode.bool
                            , Decode.map Ok <| Decode.field "need" Decode.int
                            ]
                }

        Nothing ->
            Cmd.none
