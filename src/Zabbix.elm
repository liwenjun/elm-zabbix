module Zabbix exposing
    ( RpcData, RpcTaskData, Data
    , Param
    , call, callTask
    , decoder, encode
    , flat, flatResponse, toResult, httpErrToString
    , User, defaultUser
    , login, loginTask, version, versionTask
    )

{-| 一个通用的Zabbix-API调用接口，帮助实现扩展Zabbix客户端应用。


# 数据结构

@docs RpcData, RpcTaskData, Data
@docs Param


# 调用方法

@docs call, callTask


# 辅助函数

@docs decoder, encode
@docs flat, flatResponse, toResult, httpErrToString


# Zabbix API

@docs User, defaultUser
@docs login, loginTask, version, versionTask

-}

import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E
import Task exposing (Task)



-- 数据定义


{-| JsonRpc 返回错误格式定义
-}
type alias RpcError =
    { code : Int
    , message : String
    , data : Maybe String
    }


{-| JsonRpc 响应结果
-}
type Response a
    = InnerResult a
    | InnerError RpcError


{-| Http 返回数据定义
-}
type alias WebData a =
    Result Http.Error a


{-| 基于 http 的 JsonRpc 响应结果
-}
type alias RpcData a =
    WebData (Response a)


{-| Http Task 调用返回数据定义
-}
type alias TaskData a =
    Task Http.Error a


{-| Http Task 调用 JsonRpc 响应结果
-}
type alias RpcTaskData a =
    TaskData (Response a)


{-| 便于处理的返回结果
-}
type Data a
    = RpcResult a
    | RpcErr RpcError
    | HttpErr Http.Error



-- 调用函数


{-| 调用参数
-}
type alias Param =
    { url : String
    , token : Maybe String
    , method : String
    , params : List ( String, E.Value )
    }


{-| 构建JsonRpc请求体
-}
zabbix_request : Param -> E.Value
zabbix_request opts =
    E.object
        [ ( "id", E.int 0 )
        , ( "jsonrpc", E.string "2.0" )
        , ( "method", E.string opts.method )
        , ( "params", E.object opts.params )
        , ( "auth", opts.token |> Maybe.map E.string |> Maybe.withDefault E.null )
        ]


{-| zabbix 调用方法
-}
call :
    Param
    -> D.Decoder a
    -> (RpcData a -> msg)
    -> Cmd msg
call opts dc toMsg =
    let
        dcX =
            D.oneOf
                [ D.map InnerResult (D.at [ "result" ] dc)
                , D.map InnerError (D.at [ "error" ] errorDecoder)
                ]
    in
    Http.post
        { url = opts.url
        , body = Http.jsonBody (zabbix_request opts)
        , expect = Http.expectJson toMsg dcX
        }


{-| -}
handleJsonResponse : D.Decoder a -> Http.Response String -> WebData a
handleJsonResponse dc response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case D.decodeString dc body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


{-| zabbix Task 调用方法
-}
callTask :
    Param
    -> D.Decoder a
    -> TaskData (Response a)
callTask opts dc =
    let
        dcX =
            D.oneOf
                [ D.map InnerResult (D.at [ "result" ] dc)
                , D.map InnerError (D.at [ "error" ] errorDecoder)
                ]
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = opts.url
        , body = Http.jsonBody (zabbix_request opts)
        , resolver = Http.stringResolver <| handleJsonResponse <| dcX
        , timeout = Nothing
        }



-- 辅助函数


{-| 将rpc调用的返回结果平面化处理
-}
flat : RpcData a -> Data a
flat rpc =
    case rpc of
        Ok d ->
            flatResponse d

        Err e ->
            HttpErr e


{-| 返回结果平面化处理
-}
flatResponse : Response a -> Data a
flatResponse rpc =
    case rpc of
        InnerResult v ->
            RpcResult v

        InnerError e ->
            RpcErr e


{-| 将平面化处理结果转换为Result
-}
toResult : Data a -> Result String a
toResult data =
    case data of
        RpcResult d ->
            Ok d

        RpcErr e ->
            Err <| errorToString e

        HttpErr he ->
            Err <| httpErrToString he


{-| Http.Error转换为String
-}
httpErrToString : Http.Error -> String
httpErrToString he =
    case he of
        Http.BadUrl u ->
            "无效URL => " ++ u

        Http.Timeout ->
            "超时"

        Http.NetworkError ->
            "网络出错"

        Http.BadStatus status ->
            "返回状态码" ++ String.fromInt status

        Http.BadBody reason ->
            reason


{-| 错误格式解码器
-}
errorDecoder : D.Decoder RpcError
errorDecoder =
    D.succeed RpcError
        |> required "code" D.int
        |> required "message" D.string
        |> optional "data" (D.nullable D.string) Nothing


{-| -}
errorToString : RpcError -> String
errorToString e =
    "Code: "
        ++ String.fromInt e.code
        ++ " Message: "
        ++ e.message
        ++ (case e.data of
                Just d ->
                    " Data: " ++ d

                _ ->
                    ""
           )



-- API


{-| 用户定义
-}
type alias User =
    { username : String
    , password : String
    }


{-| 用户的缺省值
-}
defaultUser : User
defaultUser =
    User "Admin" "zabbix"


{-| 用户数据解码器，用于flag加载数据
-}
decoder : D.Decoder User
decoder =
    D.map2 User
        (D.field "username" D.string)
        (D.field "password" D.string)


{-| 用户数据编码器，用于port保存数据
-}
encode : User -> E.Value
encode user =
    E.object
        [ ( "username", E.string user.username )
        , ( "password", E.string user.password )
        ]


{-| 登录
-}
login :
    { conf : { conf | url : String }
    , user : { user | username : String, password : String }
    , toMsg : RpcData String -> msg
    }
    -> Cmd msg
login opts =
    call
        { url = opts.conf.url
        , token = Nothing
        , method = "user.login"
        , params =
            [ ( "password", E.string opts.user.password )
            , ( "user", E.string opts.user.username )
            ]
        }
        D.string
        opts.toMsg


{-| 登录 Task调用方法
-}
loginTask :
    { conf : { conf | url : String }
    , user : { user | username : String, password : String }
    }
    -> RpcTaskData String
loginTask opts =
    callTask
        { url = opts.conf.url
        , token = Nothing
        , method = "user.login"
        , params =
            [ ( "password", E.string opts.user.password )
            , ( "user", E.string opts.user.username )
            ]
        }
        D.string


{-| ZabbixApi 版本
-}
version :
    { conf : { conf | url : String }
    , toMsg : RpcData String -> msg
    }
    -> Cmd msg
version opts =
    call
        { url = opts.conf.url
        , token = Nothing
        , method = "apiinfo.version"
        , params = []
        }
        D.string
        opts.toMsg


{-| ZabbixApi 版本 Task调用方法
-}
versionTask :
    { conf : { conf | url : String }
    }
    -> RpcTaskData String
versionTask opts =
    callTask
        { url = opts.conf.url
        , token = Nothing
        , method = "apiinfo.version"
        , params = []
        }
        D.string
