module Queue exposing (Model, Msg, init, update, view)

import Element exposing (Element, fill, row, spacing, text, width)
import MopidyRPC exposing (Error, request, tracklistGetTracks)
import MopidyRPC.Data as Data
import Session
import Utils exposing (button, linkButton, primary, ruled, trackToElement)


type alias Model =
    { session : Session.Data
    }


type Msg
    = None


init : Session.Data -> ( Model, Cmd Msg )
init data =
    ( { session = data
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )


view : Model -> ( String, List (Element Msg), List (Element Msg) )
view model =
    ( "Moparty Queue"
    , [ ruled [ width fill ] { data = List.map .track model.session.queue, viewf = trackToElement [] }
      ]
    , [ linkButton primary (model.session.makeUrl "/") <| text "Back" ]
    )
