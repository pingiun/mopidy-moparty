module Msg exposing (..)

import Browser
import Home
import Json.Encode as E
import MopidyRPC exposing (Error)
import MopidyRPC.Data exposing (PlaybackState, TlTrack)
import Queue
import Url


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | QueueMsg Queue.Msg
    | GotClientId Int
    | MopidyUpdate E.Value
    | GotTrackList (Result Error (List TlTrack))
    | GotState (Result Error PlaybackState)
    | GotTimePosition (Result Error (Maybe Int))
    | UpdatePosition
    | Play
    | Pause
    | Skip
    | DoAlive
    | Refresh
    | None
