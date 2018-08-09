module MouseEvent
    exposing
        ( xAsPortionOf
        , yAsPortionOf
        , xAndYAsPortionsOf
        , xRelativeTo
        , yRelativeTo
        , xAndYRelativeTo
        , Position
        )

{-| decoders for mouse events

@docs Position

@docs xAsPortionOf, yAsPortionOf , xAndYAsPortionsOf , xRelativeTo , yRelativeTo , xAndYRelativeTo

-}

import Json.Decode as JD


{-| a decoded position (relative or portional)
-}
type alias Position =
    { x : Float
    , y : Float
    }


type alias MouseEvent =
    { src : Element
    , mouse : MousePosition
    , container : Element
    }


type alias Element =
    { offsetLeft : Float
    , offsetWidth : Float
    , offsetTop : Float
    , offsetHeight : Float
    }


type alias MousePosition =
    { offsetX : Float
    , offsetY : Float
    }



-- API


{-| decode the mouse position relative to a parent element (values in px)
-}
xAndYRelativeTo : String -> JD.Decoder Position
xAndYRelativeTo id =
    mouseEventDecoder id |> JD.map relativeXAndY


{-| decode the mouse x position relative to a parent element (values in px)
-}
xRelativeTo : String -> JD.Decoder Float
xRelativeTo id =
    mouseEventDecoder id |> JD.map relativeX


{-| decode the mouse y position relative to a parent element (values in px)
-}
yRelativeTo : String -> JD.Decoder Float
yRelativeTo id =
    mouseEventDecoder id |> JD.map relativeY


{-| decode the mouse y position as a portion of a parent element (values between 0 and 1)
-}
yAsPortionOf : String -> JD.Decoder Float
yAsPortionOf id =
    mouseEventDecoder id |> JD.map yPortion


{-| decode the mouse x position as a portion of a parent element (values between 0 and 1)
-}
xAsPortionOf : String -> JD.Decoder Float
xAsPortionOf id =
    mouseEventDecoder id |> JD.map xPortion


{-| decode the mouse position as a portion of a parent element (values between 0 and 1)
-}
xAndYAsPortionsOf : String -> JD.Decoder Position
xAndYAsPortionsOf id =
    mouseEventDecoder id |> JD.map xAndYPortions



-- CALCULATIONS


relativeXAndY : MouseEvent -> Position
relativeXAndY mouseEvent =
    { x = relativeX mouseEvent
    , y = relativeY mouseEvent
    }


relativeX : MouseEvent -> Float
relativeX { src, mouse, container } =
    (src.offsetLeft - container.offsetLeft + mouse.offsetX)


relativeY : MouseEvent -> Float
relativeY { src, mouse, container } =
    (src.offsetTop - container.offsetTop + mouse.offsetY)


xAndYPortions : MouseEvent -> Position
xAndYPortions mouseEvent =
    { x = xPortion mouseEvent
    , y = yPortion mouseEvent
    }


xPortion : MouseEvent -> Float
xPortion mouseEvent =
    (relativeX mouseEvent) / mouseEvent.container.offsetWidth


yPortion : MouseEvent -> Float
yPortion mouseEvent =
    (relativeY mouseEvent) / mouseEvent.container.offsetHeight



-- DECODERS


mouseEventDecoder : String -> JD.Decoder MouseEvent
mouseEventDecoder id =
    JD.map3 MouseEvent
        (JD.field "srcElement" elementDecoder)
        (JD.at [] mousePositionDecoder)
        (JD.field "srcElement" (containerValues id))


mousePositionDecoder : JD.Decoder MousePosition
mousePositionDecoder =
    JD.map2 MousePosition
        (JD.field "offsetX" JD.float)
        (JD.field "offsetY" JD.float)


elementDecoder : JD.Decoder Element
elementDecoder =
    JD.map4 Element
        (pageLeftDecoder)
        (JD.field "offsetWidth" JD.float)
        (pageTopDecoder)
        (JD.field "offsetHeight" JD.float)


pageLeftDecoder : JD.Decoder Float
pageLeftDecoder =
    accumulateViaOffsetParentDecoder offsetLeftDecoder 0


pageTopDecoder : JD.Decoder Float
pageTopDecoder =
    accumulateViaOffsetParentDecoder offsetTopDecoder 0


offsetLeftDecoder : JD.Decoder Float
offsetLeftDecoder =
    JD.field "offsetLeft" JD.float


offsetTopDecoder : JD.Decoder Float
offsetTopDecoder =
    JD.field "offsetTop" JD.float


accumulateViaOffsetParentDecoder : JD.Decoder Float -> Float -> JD.Decoder Float
accumulateViaOffsetParentDecoder valueDecoder cumulated =
    valueDecoder
        |> JD.map (\value -> cumulated + value)
        |> JD.andThen (accumulateViaOffsetParentHelper valueDecoder)


accumulateViaOffsetParentHelper : JD.Decoder Float -> Float -> JD.Decoder Float
accumulateViaOffsetParentHelper valueDecoder cumulated =
    JD.oneOf
        [ JD.field "offsetParent" <| JD.lazy (\_ -> accumulateViaOffsetParentDecoder valueDecoder cumulated)
        , JD.succeed cumulated
        ]


containerValues : String -> JD.Decoder Element
containerValues containerId =
    JD.oneOf
        [ ifIdMatches containerId elementDecoder
        , JD.field "parentElement" <| JD.lazy (\_ -> containerValues containerId)
        ]


ifIdMatches : String -> JD.Decoder a -> JD.Decoder a
ifIdMatches targetId decoder =
    JD.field "id" JD.string
        |> JD.andThen
            (\elementId ->
                if elementId == targetId then
                    decoder
                else
                    (JD.fail "no match")
            )
