port module Main exposing (main, saveId)

import Browser
import Browser.Navigation as Nav
import Element
import Home
import Json.Decode as D
import Json.Encode as J
import MopidyRPC.Data exposing (PlaybackState(..), Update(..), updateDecoder)
import Msg exposing (Msg(..))
import Queue
import RPCUtil exposing (alive, getInit, getTrackList, pause, play, setConsume, voteSkip)
import Random
import Session
import Skeleton
import Time
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, top)
import Utils exposing (mapCdr)


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = Home Home.Model
    | Queue Queue.Model
    | NotFound Session.Data


init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init config url key =
    let
        initSession =
            Session.new
                (config
                    |> D.decodeValue (D.field "clientId" D.int)
                    |> Result.toMaybe
                )
                (config
                    |> D.decodeValue (D.field "urlPrefix" D.string)
                    |> Result.withDefault ""
                )

        ( mdl, cmd ) =
            stepUrl url
                { key = key
                , page =
                    NotFound initSession
                }

        session =
            exit mdl
    in
    (case session.clientId of
        Nothing ->
            ( mdl, Cmd.batch [ cmd, genId ] )

        Just _ ->
            ( mdl, cmd )
    )
        |> mapCdr (\command -> Cmd.batch [ command, setConsume, getInit ])


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        session =
            exit model
    in
    case message of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        HomeMsg msg ->
            case model.page of
                Home home ->
                    stepHome model (Home.update msg home)

                _ ->
                    ( model, Cmd.none )

        QueueMsg msg ->
            case model.page of
                Queue queue ->
                    stepQueue model (Queue.update msg queue)

                _ ->
                    ( model, Cmd.none )

        GotClientId id ->
            ( updateSession { session | clientId = Just id } model, saveId id )

        MopidyUpdate value ->
            case D.decodeValue updateDecoder value of
                Ok TracklistChanged ->
                    if session.state == Stopped then
                        ( model, Cmd.batch [ getTrackList, play ] )

                    else
                        ( model, getTrackList )

                Ok (TrackPlaybackPaused { tlTrack, timePosition }) ->
                    ( updateSession { session | position = timePosition, state = Paused } model, Cmd.none )

                Ok (PlaybackStateChanged { oldState, newState }) ->
                    ( model, Cmd.none )

                Ok (TrackPlaybackStarted { tlTrack }) ->
                    ( updateSession { session | position = 0, state = Playing } model, Cmd.none )

                Ok (TrackPlaybackResumed { tlTrack, timePosition }) ->
                    ( updateSession { session | position = timePosition, state = Playing } model, Cmd.none )

                Ok (TrackPlaybackEnded { tlTrack, timePosition }) ->
                    ( updateSession { session | position = timePosition, state = Stopped } model, Cmd.none )

                Ok (Seeked { timePosition }) ->
                    ( updateSession { session | position = timePosition } model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotTrackList result ->
            case result of
                Ok tracks ->
                    ( updateSession { session | queue = tracks } model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotState result ->
            case result of
                Ok state ->
                    ( updateSession { session | state = state } model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotTimePosition result ->
            case result of
                Ok (Just position) ->
                    ( updateSession { session | position = position } model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Play ->
            ( model, play )

        Pause ->
            ( model, pause )

        Skip ->
            ( model, voteSkip session )

        DoAlive ->
            ( model, alive session )

        UpdatePosition ->
            ( updateSession { session | position = session.position + 1000 } model, Cmd.none )

        None ->
            ( model, Cmd.none )


updateSession : Session.Data -> Model -> Model
updateSession data model =
    case model.page of
        Home m ->
            { model | page = Home { m | session = data } }

        Queue m ->
            { model | page = Queue { m | session = data } }

        NotFound _ ->
            { model | page = NotFound data }


stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmds ) =
    ( { model | page = Home home }
    , Cmd.map HomeMsg cmds
    )


stepQueue : Model -> ( Queue.Model, Cmd Queue.Msg ) -> ( Model, Cmd Msg )
stepQueue model ( queue, cmds ) =
    ( { model | page = Queue queue }, Cmd.map QueueMsg cmds )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        session =
            exit model
    in
    Sub.batch
        [ mopidyUpdates <| MopidyUpdate
        , Time.every (10 * 1000) <| always DoAlive
        , if session.state == Playing then
            Time.every 1000 <| always UpdatePosition

          else
            Sub.none
        , case model.page of
            Home home ->
                Sub.map HomeMsg <| Home.subscriptions home

            Queue _ ->
                Sub.none

            NotFound _ ->
                Sub.none
        ]


view : Model -> Browser.Document Msg
view model =
    let
        session =
            exit model
    in
    case model.page of
        Home home ->
            Skeleton.view session HomeMsg (Home.view home)

        Queue queue ->
            Skeleton.view session QueueMsg (Queue.view queue)

        NotFound _ ->
            { title = "Not found", body = [ Element.layout [] <| Element.text "Not Found" ] }


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

        parser =
            s session.urlPrefix
                </> oneOf
                        [ Parser.map (stepHome model <| Home.init session) top
                        , Parser.map (stepQueue model <| Queue.init session) (s "queue")
                        ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound session }
            , Cmd.none
            )


exit : Model -> Session.Data
exit model =
    case model.page of
        NotFound session ->
            session

        Home m ->
            m.session

        Queue m ->
            m.session


genId : Cmd Msg
genId =
    Random.generate GotClientId <| Random.int Random.minInt Random.maxInt


port saveId : Int -> Cmd msg


port mopidyUpdates : (J.Value -> msg) -> Sub msg
