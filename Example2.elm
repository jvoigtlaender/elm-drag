module Main (main) where

import Drag exposing (..)
import Graphics.Input
import Signal exposing (foldp)
import Text exposing (fromString)
import Graphics.Element exposing (layers, leftAligned, sizeOf)
import Graphics.Collage exposing (collage, outlined, rect, solid, toForm)
import Color exposing (black)


hover =
    Signal.mailbox Nothing


box1 =
    Graphics.Input.hoverable
        (Signal.message hover.address
            << \h ->
                if h then
                    Just 1
                else
                    Nothing
        )
        (putInBox (leftAligned (fromString "drag me around")))


box2 =
    Graphics.Input.hoverable
        (Signal.message hover.address
            << \h ->
                if h then
                    Just 2
                else
                    Nothing
        )
        (putInBox (leftAligned (fromString "and me too")))


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
                Just ( 1, MoveBy ( dx, dy ) ) ->
                    \( p1, p2 ) -> ( moveBy ( dx, dy ) p1, p2 )

                Just ( 2, MoveBy ( dx, dy ) ) ->
                    \( p1, p2 ) -> ( p1, moveBy ( dx, dy ) p2 )

                _ ->
                    identity
    in
        Signal.map
            (\( p1, p2 ) ->
                collage
                    200
                    200
                    [ Graphics.Collage.move p1 (toForm box1)
                    , Graphics.Collage.move p2 (toForm box2)
                    ]
            )
            (foldp update ( ( 0, 15 ), ( 0, -15 ) ) (trackMany Nothing hover.signal))
