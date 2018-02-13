import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Platform.Sub as Sub
import Json.Decode as J exposing
  ( string, float, null, bool, dict, oneOf
  , Value, Decoder, decodeString, decodeValue
  )
import Dict exposing (Dict)
import List exposing (take, any)
import Array exposing (Array, get, set, push, length)
import Char exposing (toCode)
import String exposing (trim)

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
  , output : Result ErrorString JSONString
  }

getfilter : Int -> Array Panel -> FilterString
getfilter index panels = get index panels |> Maybe.map .filter |> Maybe.withDefault "."

getoutput : Int -> Array Panel -> JSONString
getoutput index panels =
  get index panels
    |> Maybe.map .output
    |> Maybe.map (Result.withDefault "")
    |> Maybe.withDefault "."

getinputfor : Int -> Model -> String
getinputfor index model =
  case index of
    0 -> model.input
    _ -> getoutput (index - 1) model.panels

setfilter : Int -> FilterString -> Array Panel -> Array Panel
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

setoutput : Int -> JSONString -> Array Panel -> Array Panel
setoutput index output panels =
  case get index panels of
    Nothing -> panels
    Just p -> set index { p | output = Ok output } panels

seterror : Int -> ErrorString -> Array Panel -> Array Panel
seterror index error panels =
  case get index panels of
    Nothing -> panels
    Just p -> set index { p | output = Err error } panels

hasNonAsciiLetter : String -> Bool
hasNonAsciiLetter s =
  String.toList s
    |> any
      (\char ->
        let
          code = toCode char
        in
          (code < 65) ||
          (code > 90 && code < 97) ||
          (code > 122)
      )

init : (Model, Cmd Msg)
init =
  let model =
    { tab = View
    , input = "{\"x\": 23}"
    , panels = Array.fromList <|
      [ Panel True "." (Ok "")
      , Panel True "" (Ok "")
      ]
    }
  in
    ( model
    , applyfilter (0, model.input, (getfilter 0 model.panels))
    )


-- UPDATE

type Msg
  = SelectTab Tab
  | SetInput JSONString
  | SetFilter Int FilterString
  | SelectDictItem Int String
  | SelectListItem Int Int
  | GotResult (Int, JSONString)
  | GotError (Int, ErrorString)

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
                { upd | panels = upd.panels |> push (Panel True "" (Ok "")) }
              else -- enable the next panel
                { upd | panels = upd.panels |> enable (i + 1) }
            , applyfilter (i, (getinputfor i upd), v)
            )
    SelectDictItem paneln key ->
      if hasNonAsciiLetter key
      then update (SetFilter (paneln + 1) (".[" ++ toString key ++ "]")) model
      else update (SetFilter (paneln + 1) ("." ++ key)) model
    SelectListItem paneln index ->
      update (SetFilter (paneln + 1) (".[" ++ toString index ++ "]")) model
    GotResult (i, v) ->
      ( { model | panels = model.panels |> setoutput i v }
      , if length model.panels > i + 1
        then applyfilter ((i + 1), v, (getfilter (i + 1) model.panels))
        else Cmd.none
      )
    GotError (i, e) ->
      ( { model
          | panels = model.panels
            |> seterror i e
            |> disablefrom (i + 1)
        }
      , Cmd.none
      )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ gotresult GotResult
    , goterror GotError
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
        let
          panels = List.filter .enabled
            <| Array.toList model.panels
          npanels = List.length panels
          hidden = if npanels > 4 then npanels - 4 else 0
        in
          div [ id "panels", class "columns is-multiline" ]
            <| List.indexedMap (viewPanel hidden)
            <| panels
    ]

viewPanel : Int -> Int -> Panel -> Html Msg
viewPanel hidden i {filter, output} =
  div
    [ class "panel column is-3"
    , style <| if i < hidden then [("display", "none")] else []
    ]
    [ input [ class "input", onInput (SetFilter i), value filter ] []
    , div [ class "box" ]
      [ case output of
        Ok json -> viewJSON i json
        Err err -> div [ class "error" ] [ text err ]
      ]
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

viewJSON : Int -> JSONString -> Html Msg
viewJSON paneln json =
  case decodeString multiDecoder json of
    Ok jrepr -> case jrepr of
      JScalar scalar -> scalarView scalar
      JDict d -> table [ class "table is-fullwidth is-hoverable" ]
        [ tbody []
          <| List.map
            (\(k, v) ->
              tr [ onClick (SelectDictItem paneln k) ]
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
              tr [ onClick (SelectListItem paneln i) ]
                [ td [] [ text <| toString i ]
                , td [] [ viewValue v ]
                ]
            )
          <| l
        ]
    Err e -> text json

viewValue : Value -> Html Msg
viewValue jval =
  case decodeValue multiDecoder jval of
    Ok jrepr -> case jrepr of
      JScalar scalar -> scalarView scalar
      JDict d -> div [ class "sub dict" ]
        <| List.concat
          [ [ text "{" ]
          , List.intersperse (text ", ")
            <| List.map dictSubView
            <| take 3
            <| Dict.toList d
          , if Dict.size d > 3 then [ text ", …" ] else []
          , [ text "}" ]
          ]
      JList l -> div [ class "sub list" ]
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
dictSubView (k, _) = span [ class "key" ] [ text k ]

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
