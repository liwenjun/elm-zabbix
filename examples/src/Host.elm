module Host exposing (..)

{-| -}

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Zabbix exposing (RpcData, RpcTaskData, call, callTask)


{-| 主机数据结构定义
-}
type alias Host =
    { hostid : String
    , host : String
    , name : String
    }


hostDecoder : D.Decoder Host
hostDecoder =
    D.succeed Host
        |> required "hostid" D.string
        |> required "host" D.string
        |> required "name" D.string


{-| 调用参数
-}
params : List ( String, E.Value )
params =
    [ ( "output", E.list E.string [ "host", "name" ] )
    ]


{-| 调用方法
-}
get :
    { conf : { conf | url : String, token : Maybe String }
    , toMsg : RpcData (List Host) -> msg
    }
    -> Cmd msg
get opts =
    call
        { url = opts.conf.url
        , token = opts.conf.token
        , method = "host.get"
        , params = params
        }
        (D.list hostDecoder)
        opts.toMsg


{-| Task调用方法
-}
getTask :
    { conf : { conf | url : String, token : Maybe String }
    }
    -> RpcTaskData (List Host)
getTask opts =
    callTask
        { url = opts.conf.url
        , token = opts.conf.token
        , method = "host.get"
        , params = params
        }
        (D.list hostDecoder)
