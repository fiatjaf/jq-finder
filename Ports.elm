port module Ports exposing (..)

port applyfilter : (JSONString, Int, List FilterString) -> Cmd msg
port savepanelwidth : (Int, Int) -> Cmd msg

port gotresult : ((Int, JSONString) -> msg) -> Sub msg
port goterror : ((Int, ErrorString) -> msg) -> Sub msg

type alias FilterString = String
type alias JSONString = String
type alias ErrorString = String
