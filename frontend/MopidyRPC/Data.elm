module MopidyRPC.Data exposing (Album, Artist, PlaybackState(..), TlTrack, Track, Update(..), jsonRCPResultDecoder, playBackStateDecoder, searchResultDecoder, tlTrackDecoder, updateDecoder)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)


type PlaybackState
    = Playing
    | Paused
    | Stopped


type alias Artist =
    { uri : Maybe String
    , name : String
    }


type alias Album =
    { uri : Maybe String
    , name : String
    , artists : List Artist
    }


type alias Track =
    { uri : String
    , name : String
    , artists : List Artist
    , album : Maybe Album
    , length : Maybe Int
    }


type alias TlTrack =
    { tlid : Int
    , track : Track
    }


predicate : String -> (a -> Bool) -> Decoder a -> Decoder ()
predicate m p d =
    let
        runPredicate x =
            if p x then
                succeed ()

            else
                fail <| m
    in
    d
        |> andThen runPredicate


jsonRPCv2 : Decoder a -> Decoder a
jsonRPCv2 f =
    field "jsonrpc" string |> predicate "JSONRPC version not 2.0" (\x -> x == "2.0") |> andThen (always f)


jsonRCPResultDecoder : Decoder a -> Decoder a
jsonRCPResultDecoder d =
    jsonRPCv2 <|
        field "result" <|
            d


artistDecoder : Decoder Artist
artistDecoder =
    succeed Artist
        |> optional "uri" (nullable string) Nothing
        |> required "name" string


albumDecoder : Decoder Album
albumDecoder =
    succeed Album
        |> optional "uri" (nullable string) Nothing
        |> required "name" string
        |> optional "artists" (list artistDecoder) []


trackDecoder : Decoder Track
trackDecoder =
    succeed Track
        |> required "uri" string
        |> required "name" string
        |> optional "artists" (list artistDecoder) []
        |> optional "album" (nullable albumDecoder) Nothing
        |> optional "length" (nullable int) Nothing


playBackStateDecoder : Decoder PlaybackState
playBackStateDecoder =
    let
        toState state =
            case state of
                "playing" ->
                    succeed <| Playing

                "paused" ->
                    succeed <| Paused

                "stopped" ->
                    succeed <| Stopped

                o ->
                    fail <| "No such playbackstate: " ++ o
    in
    string
        |> andThen toState


tlTrackDecoder : Decoder TlTrack
tlTrackDecoder =
    succeed TlTrack
        |> required "tlid" int
        |> required "track" trackDecoder


searchResultDecoder : Decoder (List Track)
searchResultDecoder =
    map (Maybe.withDefault []) <| maybe <| field "tracks" <| list trackDecoder


type Update
    = TracklistChanged
    | TrackPlaybackStarted { tlTrack : TlTrack }
    | TrackPlaybackPaused { tlTrack : TlTrack, timePosition : Int }
    | TrackPlaybackResumed { tlTrack : TlTrack, timePosition : Int }
    | TrackPlaybackEnded { tlTrack : TlTrack, timePosition : Int }
    | PlaybackStateChanged { oldState : PlaybackState, newState : PlaybackState }
    | Seeked { timePosition : Int }


trackPlaybackPaused tlTrack timePosition =
    TrackPlaybackPaused { tlTrack = tlTrack, timePosition = timePosition }


playbackStateChanged oldState newState =
    PlaybackStateChanged { oldState = oldState, newState = newState }


seeked timePosition =
    Seeked { timePosition = timePosition }


trackPlaybackEnded tlTrack timePosition =
    TrackPlaybackEnded { tlTrack = tlTrack, timePosition = timePosition }


trackPlaybackResumed tlTrack timePosition =
    TrackPlaybackResumed { tlTrack = tlTrack, timePosition = timePosition }


trackPlaybackStarted tlTrack =
    TrackPlaybackStarted { tlTrack = tlTrack }


updateFromType : String -> Decoder Update
updateFromType string =
    case string of
        "tracklist_changed" ->
            succeed TracklistChanged

        "track_playback_started" ->
            map trackPlaybackStarted (field "tl_track" tlTrackDecoder)

        "track_playback_paused" ->
            map2 trackPlaybackPaused (field "tl_track" tlTrackDecoder) (field "time_position" int)

        "track_playback_resumed" ->
            map2 trackPlaybackResumed (field "tl_track" tlTrackDecoder) (field "time_position" int)

        "track_playback_ended" ->
            map2 trackPlaybackEnded (field "tl_track" tlTrackDecoder) (field "time_position" int)

        "playback_state_changed" ->
            map2 playbackStateChanged (field "old_state" playBackStateDecoder) (field "new_state" playBackStateDecoder)

        "seeked" ->
            map seeked (field "time_position" int)

        x ->
            fail <| "Invalid type: " ++ x


updateDecoder : Decoder Update
updateDecoder =
    field "event" string |> andThen updateFromType
