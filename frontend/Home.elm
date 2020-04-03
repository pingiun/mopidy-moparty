module Home exposing (Model, Msg, init, subscriptions, update, view)

import Element exposing (Element, el, fill, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import List
import MopidyRPC exposing (Error, librarySearchTrack, request, tracklistAdd)
import MopidyRPC.Data as Data exposing (PlaybackState(..), TlTrack, Track)
import Session
import Utils exposing (brighter, button, good, icon, onEnter, primary, ruled, smallButton, trackToElement)


type alias Model =
    { session : Session.Data
    , searchContent : String
    , playing : Data.PlaybackState
    , position : Maybe Int
    , searchResults : List Data.Track
    , message : String
    }


type Msg
    = None
    | GotSearchResult (Result Error (List Data.Track))
    | GotTrackAdd (Result Error (List Data.TlTrack))
    | UpdateSearch String
    | DoTrackAdd String
    | DoTrackAddNext String
    | EnterWasPressed


init : Session.Data -> ( Model, Cmd Msg )
init data =
    ( { session = data
      , searchContent = ""
      , playing = Stopped
      , position = Nothing
      , searchResults = []
      , message = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        UpdateSearch content ->
            case content of
                "" ->
                    ( { model | searchResults = [], searchContent = content }, Cmd.none )

                _ ->
                    ( { model | searchContent = content }, Cmd.none )

        EnterWasPressed ->
            case model.searchContent of
                "" ->
                    ( { model | message = "" }, Cmd.none )

                _ ->
                    ( { model | message = "Loading..." }, librarySearchTrack model.searchContent GotSearchResult |> request )

        GotSearchResult result ->
            case result of
                Ok tracks ->
                    ( { model | message = "", searchResults = tracks }, Cmd.none )

                Err _ ->
                    ( { model | message = "Error occured" }, Cmd.none )

        DoTrackAdd uri ->
            ( { model | message = "" }, addTrack uri )

        DoTrackAddNext uri ->
            ( { model | message = "" }, addTrackNext uri )

        GotTrackAdd result ->
            case ( result, model.playing ) of
                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> ( String, List (Element Msg), List (Element Msg) )
view model =
    ( "Moparty Home"
    , [ messageRow model
      , searchRow model
      , resultsRow model
      ]
    , []
    )


messageRow : Model -> Element Msg
messageRow model =
    case model.message of
        "" ->
            Element.none

        msg ->
            text msg


searchRow model =
    row [ width fill, spacing 16 ]
        [ Input.text [ Input.focusedOnLoad, onEnter EnterWasPressed ]
            { label = Input.labelHidden "Search"
            , onChange = UpdateSearch
            , placeholder = Just <| Input.placeholder [] (text "Search")
            , text = model.searchContent
            }
        , button good EnterWasPressed <| icon model.session "search" "Search"
        ]


wasAdded : Model -> Track -> Bool
wasAdded model track =
    List.filter (\x -> x.track.uri == track.uri) model.session.queue
        |> List.length
        |> (/=) 0


resultsRow : Model -> Element Msg
resultsRow model =
    ruled [ width fill ]
        { data = model.searchResults
        , viewf =
            \track ->
                if wasAdded model track then
                    trackToElement [ el [ Border.rounded 3, padding 16, Background.color <| brighter good ] (icon model.session "check" "Added") ] track

                else
                    trackToElement
                        [ button good (DoTrackAdd track.uri) <| icon model.session "plus" "Add to end"
                        , smallButton primary (DoTrackAddNext track.uri) <| icon model.session "arrow-up" "Add next"
                        ]
                        track
        }


addTrack : String -> Cmd Msg
addTrack uri =
    tracklistAdd [ uri ] Nothing GotTrackAdd
        |> request


addTrackNext : String -> Cmd Msg
addTrackNext uri =
    tracklistAdd [ uri ] (Just 1) GotTrackAdd
        |> request
