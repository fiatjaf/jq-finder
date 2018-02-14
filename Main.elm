import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Platform.Sub as Sub
import Mouse
import Json.Decode as J exposing
  ( string, float, null, bool, dict, oneOf
  , Value, Decoder, decodeString, decodeValue
  )
import Dict exposing (Dict)
import List exposing (take, any, range, intersperse, concat)
import Array exposing (Array, get, set, push, length)
import Char exposing (toCode)
import String exposing (trim, join)

import Ports exposing (..)


main =
  Html.programWithFlags
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
  , drag : Maybe (Int, Int)
  }

type Tab = Input | View

type alias Panel =
  { enabled : Bool
  , filter : FilterString
  , output : Result ErrorString JSONString
  , w : Int
  }

getfiltersuntil : Int -> Array Panel -> List FilterString
getfiltersuntil to panels =
  panels
    |> Array.toList
    |> take (to + 1)
    |> List.map .filter

joinfilters : List FilterString -> FilterString
joinfilters filters =
  ". as $input | " ++
    ( filters
      |> List.filter (trim >> (/=) "")
      |> join " | "
      |> \f -> if f /= "" then f else "."
    )

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

init : { input : String, filters : List String, widths : List Int } -> (Model, Cmd Msg)
init {input, filters, widths} =
  let model =
    { tab = View
    , input = input
    , panels = Array.fromList
      <| List.map2 (\f w -> Panel True f (Ok "") w) filters widths
    , drag = Nothing
    }
  in
    ( model
    , Cmd.batch
      <| List.map
        (\pi -> applyfilter (model.input, pi, model.panels |> getfiltersuntil pi))
      <| range 0 (Array.length model.panels)
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
  | DragStart Int Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTab t -> ({ model | tab = t }, Cmd.none)
    SetInput v ->
      ( { model | input = v }
      , Cmd.batch
          <| List.map
            (\pi -> applyfilter (model.input, pi, model.panels |> getfiltersuntil pi))
          <| range 0 (Array.length model.panels)
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
                { upd | panels = upd.panels |> push (Panel True "" (Ok "") 250) }
              else -- enable the next panel
                { upd | panels = upd.panels |> enable (i + 1) }
            , Cmd.batch
                [ Cmd.batch
                  <| List.map
                    (\pi ->
                      let filters = (upd.panels |> getfiltersuntil pi)
                      in applyfilter (model.input, pi, filters)
                    )
                  <| range i <| (Array.length upd.panels) - 1
                , scrollintopanel (i + 1)
                , case get (i + 1) upd.panels of
                    Nothing -> savepanelwidth ((i + 1), 250)
                    Just panel -> savepanelwidth ((i + 1), panel.w)
                ]
            )
    SelectDictItem paneln key ->
      if hasNonAsciiLetter key
      then update (SetFilter (paneln + 1) (".[" ++ toString key ++ "]")) model
      else update (SetFilter (paneln + 1) ("." ++ key)) model
    SelectListItem paneln index ->
      update (SetFilter (paneln + 1) (".[" ++ toString index ++ "]")) model
    GotResult (i, v) ->
      ( { model | panels = model.panels |> setoutput i v }
      , Cmd.none
      )
    GotError (i, e) ->
      ( { model
          | panels = model.panels
            |> seterror i e
            |> disablefrom (i + 1)
        }
      , Cmd.none
      )
    DragStart paneln {x} ->
      ( case get paneln model.panels of
          Just panel ->
            { model | drag = Just (paneln, x - panel.w) }
          Nothing -> model
      , Cmd.none
      )
    DragAt {x} ->
      ( case model.drag of
          Just (paneln, initx) ->
            case get paneln model.panels of
              Just panel ->
                { model | panels = model.panels |> set paneln { panel | w = x - initx } }
              Nothing -> model
          Nothing -> model
      , Cmd.none
      )
    DragEnd _ ->
      ( { model | drag = Nothing }
      , case model.drag of
          Nothing -> Cmd.none
          Just (paneln, _) ->
            case get paneln model.panels of
              Nothing -> Cmd.none
              Just panel -> savepanelwidth (paneln, panel.w)
      )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ gotresult GotResult
    , goterror GotError
    , case model.drag of
      Nothing -> Sub.none
      Just _ ->
        Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
    ]


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [ class "tabs is-centered is-boxed" ]
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
    , div [ class "container", id "resulting-filter" ]
      [ model.panels
        |> Array.toList
        |> List.map .filter
        |> joinfilters
        |> text
      ]
    , case model.tab of
      Input ->
        div [ id "input" ]
          [ textarea [ class "textarea", onInput SetInput ] [ text model.input ]
          ]
      View ->
        div [ id "panels" ]
          <| List.indexedMap viewPanel
          <| List.filter .enabled
          <| Array.toList model.panels
    ]

viewPanel : Int -> Panel -> Html Msg
viewPanel i {filter, output, w} =
  div
    [ class "panel"
    , style [("width", toString w ++ "px")]
    ]
    [ input [ class "input", onInput (SetFilter i), value filter ] []
    , div [ class "box" ]
      [ case output of
        Ok json -> viewJSON i json
        Err err -> div [ class "error" ] [ text err ]
      ]
    , div
      [ class "resize-handle"
      , on "mousedown" (J.map (DragStart i) Mouse.position)
      ] []
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
        <| concat
          [ [ text "{" ]
          , intersperse (text ", ")
            <| List.map dictSubView
            <| take 3
            <| Dict.toList d
          , if Dict.size d > 3 then [ text ", …" ] else []
          , [ text "}" ]
          ]
      JList l -> div [ class "sub list" ]
        <| concat
          [ [ text "[" ]
          , intersperse (text ", ")
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
