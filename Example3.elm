module Main (main) where

import Dict
import Drag exposing (..)
import Graphics.Input
import Signal exposing (foldp, merge)
import Text exposing (fromString)
import Graphics.Element exposing (color, down, flow, layers, leftAligned, sizeOf)
import Graphics.Collage exposing (collage, outlined, rect, solid, toForm)
import Color exposing (black, orange, yellow)


add =
    Signal.mailbox ()


button =
    Graphics.Input.button (Signal.message add.address ()) "add a draggable box"


hover =
    Signal.mailbox Nothing


makeBox i =
    Graphics.Input.hoverable
        (Signal.message hover.address
            << \h ->
                if h then
                    Just i
                else
                    Nothing
        )
        (putInBox (leftAligned (fromString (toString i))))


putInBox e =
    let
        ( sx, sy ) = sizeOf e
    in
        layers [ e, collage sx sy [ outlined (solid black) (rect (toFloat sx) (toFloat sy)) ] ]


moveBy ( dx, dy ) ( x, y ) =
    ( x + toFloat dx, y - toFloat dy )


type Event
    = Add Int
    | Track (Maybe ( Int, Action ))


main =
    let
        update event =
            case event of
                Add i ->
                    Dict.insert i ( ( 0, 0 ), color yellow (makeBox i) )

                Track (Just ( i, Lift )) ->
                    Dict.update i (Maybe.map (\( p, b ) -> ( p, color orange b )))

                Track (Just ( i, MoveBy ( dx, dy ) )) ->
                    Dict.update i (Maybe.map (\( p, b ) -> ( moveBy ( dx, dy ) p, b )))

                Track (Just ( i, Release )) ->
                    Dict.update i (Maybe.map (\( p, b ) -> ( p, color yellow b )))

                _ ->
                    identity
    in
        Signal.map
            (\dict ->
                flow
                    down
                    [ button
                    , collage 200 200 (List.map (\( p, b ) -> Graphics.Collage.move p (toForm b)) (Dict.values dict))
                    ]
            )
            (foldp
                update
                Dict.empty
                (merge
                    (Signal.map Add (foldp (\_ t -> t + 1) 0 add.signal))
                    (Signal.map Track (trackMany Nothing hover.signal))
                )
            )
