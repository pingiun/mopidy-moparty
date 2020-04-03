module Skeleton exposing (view)

import Browser
import Element exposing (Element, centerX, column, el, fill, fillPortion, link, maximum, padding, paragraph, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import MopidyRPC.Data exposing (PlaybackState(..), Track)
import Msg exposing (Msg(..))
import Session exposing (Data)
import Utils exposing (button, darker, good, icon, lengthToTime, lightGrey, normalFont, primary, smallFont, smallIcon, warn)


view : Data -> (a -> Msg) -> ( String, List (Element a), List (Element a) ) -> Browser.Document Msg
view session toMsg ( title, body, extraButtons ) =
    { title = title
    , body =
        [ Element.layout normalFont <| column [ padding 16, centerX, width (fill |> maximum 1000), spacing 16 ] <| buttonRow session (List.map (Element.map toMsg) extraButtons) :: List.map (Element.map toMsg) body ]
    }


buttonRow : Data -> List (Element Msg) -> Element Msg
buttonRow session extraButtons =
    wrappedRow [ width fill ]
        [ column [ spacing 8, width (fillPortion 1) ]
            [ row [ spacing 8 ]
                ([ case ( session.state, session.queue ) of
                    ( Playing, _ ) ->
                        button warn Pause <| icon session "pause" "Pause"

                    ( Paused, _ ) ->
                        button good Play <| icon session "play" "Play"

                    ( Stopped, [] ) ->
                        button (darker lightGrey) None <| icon session "play" "Play"

                    ( Stopped, _ ) ->
                        button good Play <| icon session "play" "Play"
                 , button primary Skip <| icon session "step-forward" "Skip current song"
                 ]
                    ++ extraButtons
                )
            ]
        , column [ spacing 8, Border.rounded 3, padding 16, width (fillPortion 1), Background.color lightGrey ]
            (case session.queue of
                [] ->
                    [ text "Nothing in queue" ]

                _ ->
                    upNext session
            )
        ]


upNext : Data -> List (Element Msg)
upNext session =
    case List.map .track session.queue of
        [] ->
            []

        first :: xs ->
            upNextElem session first
                :: (case xs of
                        [] ->
                            []

                        second :: ys ->
                            row [ spacing 8 ]
                                [ smallIcon session "arrow-right" "Up next"
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
                                                { url = session.makeUrl "/queue"
                                                , label =
                                                    row [ spacing 8 ]
                                                        [ smallIcon session "plus" "Plus"
                                                        , el smallFont <| text ((List.length more |> String.fromInt) ++ " more songs")
                                                        ]
                                                }
                                                :: []
                                   )
                   )


upNextElem : Data -> Track -> Element Msg
upNextElem model track =
    let
        played =
            lengthToTime model.position ++ " / "
    in
    paragraph []
        [ text track.name
        , el smallFont <| text <| " by " ++ (String.join " & " <| List.map .name track.artists)
        , el smallFont <| text <| " (" ++ played ++ Maybe.withDefault "?:??" (Maybe.map lengthToTime track.length) ++ ")"
        ]
