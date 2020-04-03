module RPCUtil exposing (..)

import Http
import Json.Decode as D
import Json.Encode as J
import MopidyRPC exposing (method, playbackGetState, playbackGetTimePosition, request, trackListSetConsume, tracklistGetTracks)
import Msg exposing (Msg(..))
import Session exposing (Data)


alive : Data -> Cmd Msg
alive session =
    case session.clientId of
        Just clientId ->
            Http.post
                { url = "/" ++ session.urlPrefix ++ "/api/alive"
                , body = J.object [ ( "client_id", J.int clientId ) ] |> Http.jsonBody
                , expect = Http.expectWhatever <| always None
                }

        Nothing ->
            Cmd.none


setConsume : Cmd Msg
setConsume =
    trackListSetConsume True (always None) |> request


getInit : Cmd Msg
getInit =
    Cmd.batch
        [ getTrackList
        , playbackGetState GotState |> request
        , playbackGetTimePosition GotTimePosition |> request
        ]


getTrackList =
    tracklistGetTracks GotTrackList |> request


play : Cmd Msg
play =
    request { toMsg = always None, body = method "core.playback.play", decoder = D.null () }


pause : Cmd Msg
pause =
    request { toMsg = always None, body = method "core.playback.pause", decoder = D.null () }


voteSkip : Data -> Cmd Msg
voteSkip session =
    case session.clientId of
        Just clientId ->
            Http.post
                { url = "/" ++ session.urlPrefix ++ "/api/skip"
                , body = J.object [ ( "client_id", J.int clientId ) ] |> Http.jsonBody
                , expect = Http.expectWhatever <| always None

                --Http.expectJson GotSkip <|
                --    Decode.oneOf
                --        [ Decode.map Err <| Decode.field "success" Decode.bool
                --        , Decode.map Ok <| Decode.field "need" Decode.int
                --        ]
                }

        Nothing ->
            Cmd.none
