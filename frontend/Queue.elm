module Queue exposing (Model, Msg, init, update, view)

import Element exposing (Element, fill, row, spacing, text, width)
import MopidyRPC exposing (Error, request, tracklistGetTracks)
import MopidyRPC.Data as Data
import Session
import Utils exposing (button, linkButton, primary, ruled, trackToElement)


type alias Model =
    { session : Session.Data
    , queue : Maybe (List Data.Track)
    }


type Msg
    = GotTrackList (Result Error (List Data.Track))
    | None


init : Session.Data -> ( Model, Cmd Msg )
init data =
    ( { session = data
      , queue = Nothing
      }
    , getTrackList
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTrackList result ->
            case result of
                Ok tracks ->
                    ( { model | queue = Just tracks }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        None ->
            ( model, Cmd.none )


view : Model -> ( String, List (Element Msg) )
view model =
    ( "Moparty Queue"
    , [ linkButton primary ".." <| text "Back"
      , case model.queue of
            Just tracks ->
                ruled [ width fill ] { data = tracks, viewf = trackToElement [] }

            Nothing ->
                text "Loading..."
      ]
    )


getTrackList : Cmd Msg
getTrackList =
    tracklistGetTracks GotTrackList
        |> request
