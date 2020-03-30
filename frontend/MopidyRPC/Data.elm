module MopidyRPC.Data exposing (Album, Artist, PlaybackState(..), TlTrack, Track, jsonRCPResultDecoder, playBackStateDecoder, searchResultDecoder, tlTrackDecoder, trackDecoder)

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
