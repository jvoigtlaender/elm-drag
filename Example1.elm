module Main (main) where

import Drag exposing (..)
import Graphics.Input
import Signal exposing (foldp)
import Text exposing (fromString)
import Graphics.Element exposing (layers, leftAligned, sizeOf)
import Graphics.Collage exposing (collage, outlined, rect, solid, toForm)
import Color exposing (black)


hover =
    Signal.mailbox False


box =
    Graphics.Input.hoverable
        (Signal.message hover.address)
        (putInBox (leftAligned (fromString "drag me around")))


putInBox e =
    let
        ( sx, sy ) = sizeOf e
    in
        layers [ e, collage sx sy [ outlined (solid black) (rect (toFloat sx) (toFloat sy)) ] ]


moveBy ( dx, dy ) ( x, y ) =
    ( x + toFloat dx, y - toFloat dy )


main =
    let
        update m =
            case m of
                Just (MoveBy ( dx, dy )) ->
                    moveBy ( dx, dy )

                _ ->
                    identity
    in
        Signal.map
            (\p -> collage 200 200 [ Graphics.Collage.move p (toForm box) ])
            (foldp update ( 0, 0 ) (track False hover.signal))
