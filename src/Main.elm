import Browser
import Html
import Html.Styled exposing (..)
import Html.Attributes exposing (..)
import Html.Styled.Attributes exposing (css)
import Css exposing (..)
import Task
import Time



-- MAIN


main =
  Browser.element
    { init = init
    , view = view >> toUnstyled
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
    [ css
      [ backgroundColor (hex "#7F7FD5")
      , backgroundImage (linearGradient2 toTopLeft (stop <| hex "#4776E6") (stop <| hex "#8E54E9") [])
      , minHeight <| vh 100
      , padding <| px 50
      ]
    ]
    [ h1
      [ css
        [ color <| hex "fff"
        , fontSize <| px 30
        ]
      ]
      [ text "NxTodo" ]
    ,p
      [ css
        [ color <| hex "fff"
        , fontSize <| px 100
        , lineHeight <| px 100
        , marginTop <| px 30
        ]
      ]
      [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
    ]
