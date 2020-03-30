port module Main exposing (main, saveId)

import Browser
import Browser.Navigation as Nav
import Element
import Home
import Http
import Json.Decode as D
import Json.Encode as J
import Queue
import Random
import Session
import Skeleton
import Time
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, top)


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
        ( mdl, cmd ) =
            stepUrl url
                { key = key
                , page =
                    NotFound <|
                        Session.new
                            (config
                                |> D.decodeValue (D.field "clientId" D.int)
                                |> Result.toMaybe
                            )
                            (config
                                |> D.decodeValue (D.field "urlPrefix" D.string)
                                |> Result.withDefault ""
                            )
                }

        session =
            exit mdl
    in
    case session.clientId of
        Nothing ->
            ( mdl, Cmd.batch [ cmd, genId ] )

        Just _ ->
            ( mdl, cmd )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | QueueMsg Queue.Msg
    | GotClientId Int
    | DoAlive
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
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
            let
                session =
                    exit model
            in
            ( updateSession { session | clientId = Just id } model, saveId id )

        DoAlive ->
            ( model, alive model )

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
    Sub.batch
        [ Time.every 1000 <| always DoAlive
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
    case model.page of
        Home home ->
            Skeleton.view HomeMsg (Home.view home)

        Queue queue ->
            Skeleton.view QueueMsg (Queue.view queue)

        NotFound _ ->
            { title = "Not found", body = [ Element.layout [] <| Element.text "Not Found" ] }


exit : Model -> Session.Data
exit model =
    case model.page of
        NotFound session ->
            session

        Home m ->
            m.session

        Queue m ->
            m.session


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


alive : Model -> Cmd Msg
alive model =
    let
        session =
            exit model
    in
    case session.clientId of
        Just clientId ->
            Http.post
                { url = "/" ++ session.urlPrefix ++ "/api/alive"
                , body = J.object [ ( "client_id", J.int clientId ) ] |> Http.jsonBody
                , expect = Http.expectWhatever <| always None
                }

        Nothing ->
            Cmd.none


genId : Cmd Msg
genId =
    Random.generate GotClientId <| Random.int Random.minInt Random.maxInt


port saveId : Int -> Cmd msg
