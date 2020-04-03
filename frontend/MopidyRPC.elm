module MopidyRPC exposing (..)

import Http
import Json.Decode as D
import Json.Encode as J
import MopidyRPC.Data exposing (PlaybackState, TlTrack, Track, jsonRCPResultDecoder, playBackStateDecoder, searchResultDecoder, tlTrackDecoder)


type Error
    = HttpError Http.Error
    | BadRequest D.Error


request : Request msg a -> Cmd msg
request { toMsg, body, decoder } =
    Http.post
        { url = "/mopidy/rpc"
        , body = body |> Http.jsonBody
        , expect = expectRPC toMsg decoder
        }


type alias Request msg a =
    { toMsg : Result Error a -> msg
    , body : J.Value
    , decoder : D.Decoder a
    }


expectRPC : (Result Error a -> msg) -> D.Decoder a -> Http.Expect msg
expectRPC toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (HttpError (Http.BadUrl url))

                Http.Timeout_ ->
                    Err (HttpError Http.Timeout)

                Http.NetworkError_ ->
                    Err (HttpError Http.NetworkError)

                Http.BadStatus_ metadata body ->
                    Err (HttpError (Http.BadStatus metadata.statusCode))

                Http.GoodStatus_ metadata body ->
                    case D.decodeString (jsonRCPResultDecoder decoder) body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadRequest err)


method name =
    J.object
        [ ( "jsonrpc", J.string "2.0" )
        , ( "id", J.int 1 )
        , ( "method", J.string name )
        ]


methodArgs : String -> J.Value -> J.Value
methodArgs name args =
    J.object
        [ ( "jsonrpc", J.string "2.0" )
        , ( "id", J.int 1 )
        , ( "method", J.string name )
        , ( "params", args )
        ]


playbackGetState : (Result Error PlaybackState -> msg) -> Request msg PlaybackState
playbackGetState toMsg =
    { toMsg = toMsg
    , body = method "core.playback.get_state"
    , decoder = playBackStateDecoder
    }


playbackGetTimePosition : (Result Error (Maybe Int) -> msg) -> Request msg (Maybe Int)
playbackGetTimePosition toMsg =
    { toMsg = toMsg
    , body = method "core.playback.get_time_position"
    , decoder = D.nullable D.int
    }


tracklistGetTracks : (Result Error (List TlTrack) -> msg) -> Request msg (List TlTrack)
tracklistGetTracks toMsg =
    { toMsg = toMsg
    , body = method "core.tracklist.get_tl_tracks"
    , decoder = D.list tlTrackDecoder
    }


tracklistAdd : List String -> Maybe Int -> (Result Error (List TlTrack) -> msg) -> Request msg (List TlTrack)
tracklistAdd uris pos toMsg =
    let
        position =
            case pos of
                Just x ->
                    [ ( "at_position", J.int x ) ]

                Nothing ->
                    []
    in
    { toMsg = toMsg
    , body = methodArgs "core.tracklist.add" <| J.object ([ ( "uris", J.list J.string uris ) ] ++ position)
    , decoder = D.list tlTrackDecoder
    }


librarySearchTrack : String -> (Result Error (List Track) -> msg) -> Request msg (List Track)
librarySearchTrack query toMsg =
    { toMsg = toMsg
    , body = methodArgs "core.library.search" <| J.object [ ( "query", J.object [ ( "any", J.list J.string <| String.split " " query ) ] ) ]

    -- Flatten list of search results
    , decoder = D.map List.concat <| D.list searchResultDecoder
    }


trackListSetConsume : Bool -> (Result Error () -> msg) -> Request msg ()
trackListSetConsume consume toMsg =
    { toMsg = toMsg
    , body = methodArgs "core.tracklist.set_consume" <| J.list J.bool [ consume ]
    , decoder = D.null ()
    }
