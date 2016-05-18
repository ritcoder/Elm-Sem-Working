module App.Components.Lookups.IdTypesPanel exposing (..)



import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import App.Context as AppContext
import Notification exposing (info)
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { context : AppContext.Model }


init : AppContext.Model -> Model
init context =
  { context = context }


name : String
name =
  "Id Types"


-- UPDATE
type Msg
  = NoOp
  
  | Busy String
  | Idle
  | Close


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    _ ->
      model |> noFx


-- VIEW
view : AppContext.Model -> Model -> Html Msg
view context model =
  div [ class "ui info icon message" ]
    [ i [ class "notched circle loading icon" ] []
    , div [ class "content" ]
        [ div [ class "content" ]
            [ div [ class "header" ] [ text "Just a moment" ]
            , p [] [ text "Men at work! Please come back later" ]
            ]
        ]
    ]


-- COMPONENT
type alias Container c =
  { c | userSettings : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .userSettings (\x m -> { m | userSettings = x})


observe :
  { b | busy : String -> a, close: a, idle: a }
  -> Msg
  -> Maybe a
observe observer msg = 
  case msg of
    Idle ->
      Just observer.idle
    
    Busy msg ->
      Just <| observer.busy msg
    
    Close ->
      Just observer.close
    
    _ ->
      Nothing


partView :
  AppContext.Model
  -> (Parts.Msg (Container a) b -> b)
  -> (Parts.ContextualView AppContext.Model (Container a) b -> d)
  -> { e | busy : String -> b, close: b, idle: b }
  -> ( d, Cmd b )
partView context msgTagger toPartView observer =
  let
    model =
      init context
    instance =
      part msgTagger model [ observe observer ]
  in 
    (toPartView instance.view, Cmd.none)


winConfig =
  { icon = "list layout", title = "Identification Types", subTitle = "",  width = 450, height = 0 }