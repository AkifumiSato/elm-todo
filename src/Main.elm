import Browser
import Html.Styled as Styled
import Html.Styled.Attributes exposing (css, placeholder, value)
import Html.Styled.Events exposing (onSubmit, onInput)
import Css exposing (..)
import Css.Transitions exposing (transition)
import Css.Animations exposing (custom, keyframes, property)
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
  , date : Time.Posix
  }


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , todos : List Todo
  , userInput : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) [] "test"
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | Add String
  | Input String


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

    Add input ->
      ( { model | todos = ( Todo input model.time ) :: model.todos }
      , Cmd.none
      )

    Input input ->
      ( { model | userInput = input }
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
        [ onSubmit (Add model.userInput)
        , css
          [ marginTop (px 30)
          , color (hex "ccc")
          , fontSize (px 16)
          , lineHeight (px 16)
          ]
        ]
        [ Styled.p [] [ Styled.text "Write your new Todo." ]
        , Styled.input
          [ onInput Input
          , value model.userInput
          , css
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
    [ css
      [ marginTop (px 30)
      ]
    ]
    [ Styled.ul
      []
      ( List.map (\todo -> Styled.li
        [ css
          [ boxSizing borderBox
          , backgroundColor (hex "fff")
          , borderRadius (px 3)
          , boxShadow4 (px 0) (px 4) (px 24) (rgba 0 0 0 0.15)
          , color (hex "aaa")
          , fontSize (px 20)
          , padding (px 20)
          , width (px 500)
          , marginTop (px 20)
          , transform (translateY (px 0))
          , transition
            [ Css.Transitions.boxShadow 500
            , Css.Transitions.transform 500
            ]
          , firstChild
            [ marginTop (px 0)
            ]
          , hover
            [ boxShadow4 (px 0) (px 4) (px 48) (rgba 0 0 0 0.3)
            , transform (translateY (px -3))
            ]
          ]
        ]
        [ Styled.text todo.title ]) todos
      )
    ]
