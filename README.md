# elm-mouse-decoders

Decoders for extracting information from mouse events

## Relative position

Decode the position of the mouse in relation to a parent element.

```elm
import MouseEvent

type alias Px = Px

type Msg
  = MouseMoved {x : Px, y : Px}

...

containerView : Model -> Html Msg
containerView model =
    div
        [ id "abc"
        , on "mousemove" (JD.map MouseMoved <| MouseEvent.xAndYRelativeTo "abc")
        ]
        []
```

## Portional position

Decode the position of the mouse in proportion to a parent element.

```elm
import MouseEvent


type alias Portion = Float

type Msg
  = MouseMoved {x : Portion, y : Portion}

...

containerView : Model -> Html Msg
containerView model =
    div
        [ id "abc"
        , on "mousemove" (JD.map MouseMoved <| MouseEvent.xAndYAsPortionsOf "abc")
        ]
        []
```
