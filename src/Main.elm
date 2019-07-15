import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0)
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    hour   = String.fromInt (Time.toHour   model.zone model.time)
    minute = String.fromInt (Time.toMinute model.zone model.time)
    second = String.fromInt (Time.toSecond model.zone model.time)
  in
  div
    [ style "background" "#7F7FD5"
      ,style "background" "-webkit-linear-gradient(to right, #91EAE4, #86A8E7, #7F7FD5)"
      ,style "background" "linear-gradient(to right, #91EAE4, #86A8E7, #7F7FD5)"
    ]
    [
      h1 [] [ text "Nxt" ]
      ,p [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
      ,p [] [ text "hello, world" ]
    ]
