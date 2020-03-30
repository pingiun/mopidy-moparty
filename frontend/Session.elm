module Session exposing (..)


type alias Data =
    { clientId : Maybe Int
    , urlPrefix : String
    }


new : Maybe Int -> String -> Data
new clientId urlPrefix =
    { clientId = clientId, urlPrefix = urlPrefix }
