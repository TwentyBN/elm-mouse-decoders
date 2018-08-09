module RelativePositionInContainer exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import MouseEvent
import Json.Decode as JD


type Msg
    = MouseMoved Position


type alias Model =
    { relativePosition : Position
    }


type alias Position =
    { x : Float
    , y : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { relativePosition = Position 0 0 }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMoved relativePosition ->
            ( { model | relativePosition = relativePosition }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ style [ ( "height", "100%" ) ] ]
        [ containerView model
        , valuesView model
        ]


valuesView : Model -> Html Msg
valuesView model =
    table
        [ style
            [ ( "position", "absolute" )
            , ( "top", "60%" )
            , ( "left", "10%" )
            , ( "font-size", "2em" )
            ]
        ]
        [ valueRow "relativeX" model.relativePosition.x
        , valueRow "relativeY" model.relativePosition.y
        ]


valueRow : String -> a -> Html Msg
valueRow label value =
    tr []
        [ th [ style [ ( "padding-right", "10px" ) ] ] [ text label ]
        , td [ style [ ( "text-align", "left" ) ] ] [ text <| toString <| value ]
        ]


containerView : Model -> Html Msg
containerView model =
    div []
        [ div
            [ id "abc"
            , on "mousemove" (JD.map MouseMoved <| MouseEvent.xAndYRelativeTo "abc")
            , style
                [ ( "background-color", "lightblue" )
                , ( "position", "absolute" )
                , ( "top", "10%" )
                , ( "bottom", "50%" )
                , ( "left", "10%" )
                , ( "right", "10%" )
                ]
            ]
            []
        ]


main =
    Html.program
        { view = view
        , subscriptions = \_ -> Sub.none
        , init = init
        , update = update
        }
