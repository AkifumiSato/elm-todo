import Browser
import Html.Styled as Styled
import Html.Styled.Attributes exposing (css, placeholder)
import Html.Styled.Events exposing (onSubmit)
import Css exposing (..)
import Css.Transitions exposing (transition)
import Task
import Time
import String exposing (String)



-- MAIN


main =
  Browser.element
    { init = init
    , view = view >> Styled.toUnstyled
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Todo =
  { title : String
  , date : String
  }


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , todos : List Todo
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) []
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | Add


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

    Add ->
      ( { model | todos = ( Todo "Todoタイトル" "Todo詳細" ) :: model.todos }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- VIEW


view : Model -> Styled.Html Msg
view model =
  let
    zeroPadding = String.right 2
    hour   = zeroPadding <| "0" ++ String.fromInt (Time.toHour   model.zone model.time)
    minute = zeroPadding <| "0" ++ String.fromInt (Time.toMinute model.zone model.time)
    second = zeroPadding <| "0" ++ String.fromInt (Time.toSecond model.zone model.time)
  in
  Styled.div
    [ css
      [ backgroundColor (hex "#7F7FD5")
      , backgroundImage (linearGradient2 toTopLeft (stop <| hex "#4776E6") (stop <| hex "#8E54E9") [])
      ]
    ]
    [ Styled.div
      [ css
        [ boxSizing borderBox
        , minHeight (vh 100)
        , margin2 (px 0) auto
        , paddingTop (px 50)
        , width (vw 70)
        ]
      ]
      [ Styled.h1
        [ css
          [ color (hex "fff")
          , fontSize (px 30)
          ]
        ]
        [ Styled.text "NxTodo" ]
      , Styled.p
        [ css
          [ color (hex "fff")
          , fontSize (px 100)
          , lineHeight (px 100)
          , marginTop (px 30)
          ]
        ]
        [ Styled.text (hour ++ ":" ++ minute ++ ":" ++ second) ]
      , Styled.form
        [ onSubmit Add
        , css
          [ marginTop (px 30)
          , color (hex "ccc")
          , fontSize (px 16)
          , lineHeight (px 16)
          ]
        ]
        [ Styled.p [] [ Styled.text "Write your new Todo." ]
        , Styled.input
          [ css
            [ backgroundColor transparent
            , borderBottom3 (px 1) solid (hex "fff")
            , color (hex "fff")
            , fontSize (px 20)
            , lineHeight (px 20)
            , padding (px 10)
            , width (px 500)
            ]
          ]
          []
        ]
      , viewList model.todos
      ]
    ]


viewList : List Todo -> Styled.Html Msg
viewList todos =
  Styled.div
    []
    [ Styled.li [] ( List.map (\todo -> Styled.li [] [ Styled.text todo.title ]) todos )
    ]
