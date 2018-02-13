port module Ports exposing (..)

port applyfilter : (Int, JSONString, FilterString) -> Cmd msg

port gotresult : ((Int, JSONString) -> msg) -> Sub msg
port goterror : ((Int, ErrorString) -> msg) -> Sub msg

type alias FilterString = String
type alias JSONString = String
type alias ErrorString = String
