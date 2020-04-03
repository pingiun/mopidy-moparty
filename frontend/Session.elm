module Session exposing (..)

import MopidyRPC.Data exposing (PlaybackState(..), TlTrack)


type alias Data =
    { clientId : Maybe Int
    , urlPrefix : String
    , position : Int
    , state : PlaybackState
    , queue : List TlTrack
    , makeUrl : String -> String
    }


new : Maybe Int -> String -> Data
new clientId urlPrefix =
    { clientId = clientId
    , urlPrefix = urlPrefix
    , position = 0
    , state = Stopped
    , queue = []
    , makeUrl = \url -> "/" ++ urlPrefix ++ url
    }
