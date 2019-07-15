import Browser
import Html
import Html.Styled exposing (..)
import Html.Attributes exposing (..)
import Html.Styled.Attributes exposing (css, placeholder)
import Css exposing (..)
import Css.Transitions exposing (transition)
import Task
import Time
import String exposing (String)



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
    zeroPadding = String.right 2
    hour   = zeroPadding <| "0" ++ String.fromInt (Time.toHour   model.zone model.time)
    minute = zeroPadding <| "0" ++ String.fromInt (Time.toMinute model.zone model.time)
    second = zeroPadding <| "0" ++ String.fromInt (Time.toSecond model.zone model.time)
  in
  div
    [ css
      [ backgroundColor (hex "#7F7FD5")
      , backgroundImage (linearGradient2 toTopLeft (stop <| hex "#4776E6") (stop <| hex "#8E54E9") [])
      , minHeight (vh 100)
      , padding (px 50)
      ]
    ]
    [ h1
      [ css
        [ color (hex "fff")
        , fontSize (px 30)
        ]
      ]
      [ text "NxTodo" ]
    , p
      [ css
        [ color (hex "fff")
        , fontSize (px 100)
        , lineHeight (px 100)
        , marginTop (px 30)
        ]
      ]
      [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
    , div
      [ css
        [ marginTop (px 30)
        , color (hex "ccc")
        , fontSize (px 16)
        , lineHeight (px 16)
        ]
      ]
      [ p [] [ text "Write your new Todo." ]
      , input
        [ css
          [ backgroundColor transparent
          , borderBottom3 (px 1) solid (hex "fff")
          , color (hex "fff")
          , fontSize (px 20)
          , lineHeight (px 20)
          , padding (px 10)
          , Css.width (px 500)
          ]
        ]
        []
      ]
    ]
