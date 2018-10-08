module MouseEvent exposing
    ( Position
    , xAsPortionOf, yAsPortionOf, xAndYAsPortionsOf, xRelativeTo, yRelativeTo, xAndYRelativeTo
    , ancestorNode, ancestorWithId
    )

{-| decoders for mouse events

@docs Position

@docs xAsPortionOf, yAsPortionOf, xAndYAsPortionsOf, xRelativeTo, yRelativeTo, xAndYRelativeTo

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


type alias ContainerPredicate =
    JD.Decoder Bool



-- API


ancestorWithId : String -> JD.Decoder Bool
ancestorWithId targetId =
    JD.field "id" JD.string |> JD.andThen (\elementId -> JD.succeed <| elementId == targetId)


ancestorWithData : String -> String -> JD.Decoder Bool
ancestorWithData key targetValue =
    JD.field ("data-" ++ key) JD.string |> JD.andThen (\elementValue -> JD.succeed <| elementValue == targetValue)


ancestorNode : String -> JD.Decoder Bool
ancestorNode nodeName =
    JD.field "nodeName" JD.string |> JD.andThen (\elementNodeName -> JD.succeed <| String.toUpper elementNodeName == String.toUpper nodeName)


{-| decode the mouse position relative to a parent element (values in px)
-}
xAndYRelativeTo : ContainerPredicate -> JD.Decoder Position
xAndYRelativeTo containerPredicate =
    mouseEventDecoder containerPredicate |> JD.map relativeXAndY


{-| decode the mouse x position relative to a parent element (values in px)
-}
xRelativeTo : ContainerPredicate -> JD.Decoder Float
xRelativeTo containerPredicate =
    mouseEventDecoder containerPredicate |> JD.map relativeX


{-| decode the mouse y position relative to a parent element (values in px)
-}
yRelativeTo : ContainerPredicate -> JD.Decoder Float
yRelativeTo container =
    mouseEventDecoder container |> JD.map relativeY


{-| decode the mouse y position as a portion of a parent element (values between 0 and 1)
-}
yAsPortionOf : ContainerPredicate -> JD.Decoder Float
yAsPortionOf containerPredicate =
    mouseEventDecoder containerPredicate |> JD.map yPortion


{-| decode the mouse x position as a portion of a parent element (values between 0 and 1)
-}
xAsPortionOf : ContainerPredicate -> JD.Decoder Float
xAsPortionOf containerPredicate =
    mouseEventDecoder containerPredicate |> JD.map xPortion


{-| decode the mouse position as a portion of a parent element (values between 0 and 1)
-}
xAndYAsPortionsOf : ContainerPredicate -> JD.Decoder Position
xAndYAsPortionsOf containerPredicate =
    mouseEventDecoder containerPredicate |> JD.map xAndYPortions



-- CALCULATIONS


relativeXAndY : MouseEvent -> Position
relativeXAndY mouseEvent =
    { x = relativeX mouseEvent
    , y = relativeY mouseEvent
    }


relativeX : MouseEvent -> Float
relativeX { src, mouse, container } =
    src.offsetLeft - container.offsetLeft + mouse.offsetX


relativeY : MouseEvent -> Float
relativeY { src, mouse, container } =
    src.offsetTop - container.offsetTop + mouse.offsetY


xAndYPortions : MouseEvent -> Position
xAndYPortions mouseEvent =
    { x = xPortion mouseEvent
    , y = yPortion mouseEvent
    }


xPortion : MouseEvent -> Float
xPortion mouseEvent =
    relativeX mouseEvent / mouseEvent.container.offsetWidth


yPortion : MouseEvent -> Float
yPortion mouseEvent =
    relativeY mouseEvent / mouseEvent.container.offsetHeight



-- DECODERS


mouseEventDecoder : ContainerPredicate -> JD.Decoder MouseEvent
mouseEventDecoder containerPredicate =
    JD.map3 MouseEvent
        (JD.field "srcElement" elementDecoder)
        (JD.at [] mousePositionDecoder)
        (JD.field "srcElement" (containerValues containerPredicate))


mousePositionDecoder : JD.Decoder MousePosition
mousePositionDecoder =
    JD.map2 MousePosition
        (JD.field "offsetX" JD.float)
        (JD.field "offsetY" JD.float)


elementDecoder : JD.Decoder Element
elementDecoder =
    JD.map4 Element
        pageLeftDecoder
        (JD.field "offsetWidth" JD.float)
        pageTopDecoder
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


containerValues : ContainerPredicate -> JD.Decoder Element
containerValues containerPredicate =
    JD.oneOf
        [ ifThen containerPredicate elementDecoder
        , JD.field "parentElement" <| JD.lazy (\_ -> containerValues containerPredicate)
        ]


ifThen : ContainerPredicate -> JD.Decoder a -> JD.Decoder a
ifThen containerPredicate decoder =
    containerPredicate
        |> JD.andThen
            (\isTrue ->
                if isTrue then
                    decoder

                else
                    JD.fail "no match"
            )
