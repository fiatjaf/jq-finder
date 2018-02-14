port module Ports exposing (..)

port applyfilter : (String, Int, List String) -> Cmd msg
port scrollintopanel : Int -> Cmd msg
port savepanelwidth : (Int, Int) -> Cmd msg

port gotresult : ((Int, String) -> msg) -> Sub msg
port goterror : ((Int, String) -> msg) -> Sub msg
