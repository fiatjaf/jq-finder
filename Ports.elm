port module Ports exposing (..)

port applyfilter_ : (Int, JSONString, FilterString) -> Cmd msg

port gotresult : ((Int, JSONString) -> msg) -> Sub msg

applyfilter args = applyfilter_ (Debug.log "jq" args) 

type alias FilterString = String
type alias JSONString = String
