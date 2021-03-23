module Basic exposing (main)

{-| -}

import Browser
import Host
import Html exposing (Html, button, div, pre, text)
import Html.Events exposing (onClick)
import Http
import Zabbix



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { url : String
    , token : Maybe String
    , err : Maybe String
    , hosts : List Host.Host
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model "http://127.0.0.1:8081/api_jsonrpc.php" Nothing Nothing []
    in
    ( model
    , Zabbix.login
        { conf = model
        , user = { username = "Admin", password = "zabbix" }
        , toMsg = GotToken
        }
    )



-- UPDATE


type Msg
    = GotToken (Zabbix.RpcData String)
    | GotHosts (Zabbix.RpcData (List Host.Host))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotToken data ->
            case data |> Zabbix.flat |> Zabbix.toResult of
                Err err ->
                    ( { model | err = Just err }, Cmd.none )

                Ok token ->
                    let
                        m =
                            { model | token = Just token, err = Nothing }
                    in
                    ( m
                    , Host.get
                        { conf = m
                        , toMsg = GotHosts
                        }
                    )

        GotHosts data ->
            case data |> Zabbix.flat |> Zabbix.toResult of
                Err err ->
                    ( { model | err = Just err }, Cmd.none )

                Ok d ->
                    ( { model | hosts = d, err = Nothing }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ case model.err of
            Just err ->
                text err

            Nothing ->
                text ("主机数量 = " ++ (model.hosts |> List.length |> String.fromInt))
        ]
