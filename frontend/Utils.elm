module Utils exposing (..)

import Element exposing (Element, column, el, fill, height, link, padding, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Json.Decode as Decode
import MopidyRPC.Data exposing (TlTrack, Track)
import Session exposing (Data)


normalFont =
    [ Font.size 18 ]


smallFont =
    [ Font.size 14 ]


grey =
    rgb255 186 189 182


lightGrey =
    rgb255 240 240 240


good =
    rgb255 164 224 46


primary =
    rgb255 82 161 204


warn =
    rgb255 240 48 112


icon : Data -> String -> String -> Element msg
icon session src desc =
    Element.image [ width <| px 16, height <| px 16 ] { src = session.makeUrl <| "/img/" ++ src ++ ".svg", description = desc }


smallIcon : Data -> String -> String -> Element msg
smallIcon session src desc =
    Element.image [ width <| px 8, height <| px 8 ] { src = session.makeUrl <| "/img/" ++ src ++ ".svg", description = desc }


hoverColor color =
    [ Background.color color, Element.mouseOver [ Background.color <| darker color ] ]


button color onPress label =
    Input.button ([ Border.rounded 3, padding 16 ] ++ hoverColor color) { onPress = Just onPress, label = label }


smallButton color onPress label =
    Input.button ([ Border.rounded 3, padding 8 ] ++ hoverColor color) { onPress = Just onPress, label = label }


linkButton color href label =
    link ([ Border.rounded 3, padding 16 ] ++ hoverColor color) { url = href, label = label }


lengthToTime millis =
    let
        length =
            millis // 1000

        seconds =
            modBy 60 length |> String.fromInt
    in
    (length // 60 |> String.fromInt)
        ++ ":"
        ++ (if String.length seconds == 1 then
                "0" ++ seconds

            else
                seconds
           )


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


darker color =
    Element.toRgb color
        |> (\{ red, green, blue, alpha } -> { red = red * 0.9, green = green * 0.9, blue = blue * 0.9, alpha = alpha })
        |> Element.fromRgb


brighter color =
    Element.toRgb color
        |> (\{ red, green, blue, alpha } -> { red = red * 1.1, green = green * 1.1, blue = blue * 1.1, alpha = alpha })
        |> Element.fromRgb


ruled : List (Element.Attribute msg) -> { data : List record, viewf : record -> Element msg } -> Element msg
ruled attrs { data, viewf } =
    case data of
        item :: xs ->
            column (attrs ++ [ Border.width 1, Border.rounded 3, Border.color grey, width fill ])
                (el [ width fill, padding 16 ] (viewf item)
                    :: List.map
                        (\x ->
                            el
                                [ width fill
                                , padding 16
                                , Border.color grey
                                , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                                ]
                                (viewf x)
                        )
                        xs
                )

        [] ->
            column attrs []


trackToElement : List (Element msg) -> Track -> Element msg
trackToElement buttons track =
    row [ spacing 16, width fill ]
        (buttons
            ++ [ column [ spacing 8, width fill ]
                    [ paragraph [ spacing 8, width fill ]
                        [ text track.name
                        , el smallFont <| text <| " (" ++ Maybe.withDefault "?:??" (Maybe.map lengthToTime track.length) ++ ")"
                        ]
                    , paragraph (width fill :: smallFont) [ text <| Maybe.withDefault "" (Maybe.map .name track.album) ++ " by " ++ (String.join " & " <| List.map .name track.artists) ]
                    ]
               ]
        )


mapCar : (a -> c) -> ( a, b ) -> ( c, b )
mapCar f ( x, y ) =
    ( f x, y )


mapCdr : (b -> c) -> ( a, b ) -> ( a, c )
mapCdr f ( x, y ) =
    ( x, f y )
