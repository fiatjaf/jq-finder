import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Platform.Sub as Sub
import Json.Decode as J exposing
  ( string, float, null, bool, dict, oneOf
  , Value, Decoder, decodeString, decodeValue
  )
import Dict exposing (Dict)
import List exposing (take)
import Array exposing (Array, get, set, push, length)

import Ports exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { tab : Tab
  , input : JSONString
  , panels : Array Panel
  }

type Tab = Input | View

type alias Panel =
  { enabled : Bool
  , filter : FilterString
  , output : JSONString
  }

getfilter : Int -> Array Panel -> String
getfilter index panels = get index panels |> Maybe.map .filter |> Maybe.withDefault "."

getoutput : Int -> Array Panel -> String
getoutput index panels = get index panels |> Maybe.map .output |> Maybe.withDefault "."

getinputfor : Int -> Model -> String
getinputfor index model =
  case index of
    0 -> model.input
    _ -> getoutput (index - 1) model.panels

setfilter : Int -> String -> Array Panel -> Array Panel
setfilter index filter panels =
  case get index panels of
    Nothing -> panels
    Just p -> set index { p | filter = filter } panels

enable : Int -> Array Panel -> Array Panel
enable index panels =
  case get index panels of
    Nothing -> panels
    Just p -> set index { p | enabled = True } panels

disablefrom : Int -> Array Panel -> Array Panel
disablefrom index panels =
  Array.indexedMap (\i p -> if i >= index then { p | enabled = False } else p) panels

setoutput : Int -> String -> Array Panel -> Array Panel
setoutput index output panels =
  case get index panels of
    Nothing -> panels
    Just p -> set index { p | output = output } panels

init : (Model, Cmd Msg)
init =
  let model =
    { tab = View
    , input = "{\"x\": 23}"
    , panels = Array.fromList <|
      [ Panel True "." ""
      , Panel True "" ""
      ]
    }
  in
    ( model
    , applyfilter (0, model.input, (getfilter 0 model.panels))
    )


-- UPDATE

type Msg
  = SelectTab Tab
  | SetInput String
  | SetFilter Int String
  | GotResult (Int, String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTab t -> ({ model | tab = t }, Cmd.none)
    SetInput v ->
      ( { model | input = v }
      , applyfilter (0, v, (getfilter 0 model.panels))
      )
    SetFilter i v ->
      let upd = { model | panels = model.panels |> setfilter i v }
      in
        case v of
          "" -> -- erase this output and disable all panels next
            ( { upd | panels = upd.panels
                |> setoutput i ""
                |> disablefrom (i + 1)
              }
            , Cmd.none
            )
          _ ->
            ( if length upd.panels == i + 1
              then -- add a new panel
                { upd | panels = upd.panels |> push (Panel True "" "") }
              else -- enable the next panel
                { upd | panels = upd.panels |> enable (i + 1) }
            , applyfilter (i, (getinputfor i upd), v)
            )
    GotResult (i, v) ->
      ( { model | panels = setoutput i v model.panels }
      , if length model.panels > i + 1
        then applyfilter ((i + 1), v, (getfilter (i + 1) model.panels))
        else Cmd.none
      )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ gotresult GotResult
    ]


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [ class "tabs" ]
      [ ul []
        [ li [ class <| if model.tab == Input then "is-active" else "" ]
          [ a [ onClick (SelectTab Input) ]
            [ text "Input"
            ]
          ]
        , li [ class <| if model.tab == View then "is-active" else "" ]
          [ a [ onClick (SelectTab View) ]
            [ text "View"
            ]
          ]
        ]
      ]
    , case model.tab of
      Input ->
        div [ id "input" ]
          [ textarea [ class "textarea", onInput SetInput ] [ text model.input ]
          ]
      View ->
         div [ id "panels", class "columns is-multiline" ]
           <| List.indexedMap viewPanel
           <| List.filter .enabled
           <| Array.toList model.panels
    ]

viewPanel : Int -> Panel -> Html Msg
viewPanel i {filter, output} =
  div [ class "panel column" ]
    [ input [ class "input", onInput (SetFilter i), value filter ] []
    , div [ class "box" ] [ viewJSON output ]
    ]


type JRepr
  = JScalar JScalarRepr 
  | JDict (Dict String Value)
  | JList (List Value)

type JScalarRepr
  = JNull
  | JString String
  | JBool Bool
  | JNum Float

multiDecoder : Decoder JRepr
multiDecoder = oneOf
  [ J.map JScalar <| J.map JString string
  , J.map JScalar <| J.map JBool bool
  , J.map JScalar <| J.map JNum float
  , J.map JScalar <| null JNull
  , J.map JDict (dict J.value)
  , J.map JList (J.list J.value)
  ]

viewJSON : JSONString -> Html Msg
viewJSON json =
  if String.trim json == ""
  then text ""
  else
    case decodeString multiDecoder json of
      Ok jrepr -> case jrepr of
        JScalar scalar -> scalarView scalar
        JDict d -> table [ class "table is-fullwidth is-hoverable" ]
          [ tbody []
            <| List.map
              (\(k, v) ->
                tr []
                  [ td [] [ text k ]
                  , td [] [ viewValue v ]
                  ]
              )
            <| Dict.toList d
          ]
        JList l -> table [ class "table is-fullwidth is-hoverable" ]
          [ tbody []
            <| List.indexedMap
              (\i v ->
                tr []
                  [ td [] [ text <| toString i ]
                  , td [] [ viewValue v ]
                  ]
              )
            <| l
          ]
      Err e -> text e

viewValue : Value -> Html Msg
viewValue jval =
  case decodeValue multiDecoder jval of
    Ok jrepr -> case jrepr of
      JScalar scalar -> scalarView scalar
      JDict d -> div [ class "sub" ]
        <| List.concat
          [ [ text "{" ]
          , List.intersperse (text ", ")
            <| List.map dictSubView
            <| take 3
            <| Dict.toList d
          , if Dict.size d > 3 then [ text ", …" ] else []
          , [ text "}" ]
          ]
      JList l -> div [ class "sub" ]
        <| List.concat
          [ [ text "[" ]
          , List.intersperse (text ", ")
            <| List.map arraySubView
            <| take 3
            <| l
          , if List.length l > 3 then [ text ", …" ] else []
          , [ text "]" ]
          ]
    Err e -> text e

dictSubView : (String, Value) -> Html Msg
dictSubView (k, _) = span [ class "string" ] [ text k ]

arraySubView : Value -> Html Msg
arraySubView jval =
  case decodeValue multiDecoder jval of
    Ok jrepr -> case jrepr of
      JScalar scalar -> scalarView scalar
      JDict _ -> text "{}"
      JList _ -> text "[]"
    Err e -> text e

scalarView : JScalarRepr -> Html Msg
scalarView jrepr = case jrepr of
  JNull -> span [ class "null" ] [ text "null" ]
  JBool b -> span [ class "bool" ] [ text <| if b then "true" else "false" ]
  JNum n -> span [ class "num" ] [ text <| toString n ]
  JString s -> span [ class "string" ] [ text <| "\"" ++ s ++ "\"" ]
