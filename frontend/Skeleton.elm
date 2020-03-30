module Skeleton exposing (view)

import Browser
import Element exposing (Element, centerX, column, fill, maximum, padding, spacing, width)
import Utils exposing (normalFont)


view : (a -> msg) -> ( String, List (Element a) ) -> Browser.Document msg
view toMsg ( title, body ) =
    { title = title
    , body =
        [ Element.layout normalFont <| Element.map toMsg <| column [ padding 16, centerX, width (fill |> maximum 1000), spacing 16 ] body ]
    }
