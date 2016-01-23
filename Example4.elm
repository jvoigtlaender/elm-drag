module Main (main) where

import Dict
import Automaton exposing ((>>>))
import Drag exposing (Input(..), Action(..))
import Mouse
import Signal exposing (foldp, merge)
import Graphics.Collage exposing (circle, collage, outlined, solid)
import Color exposing (black)


moveBy ( dx, dy ) ( x, y ) =
    ( x + toFloat dx, y - toFloat dy )


main =
    let
        within ( x, y ) ps =
            case ps of
                [] ->
                    Nothing

                ( i, ( cx, cy ) ) :: ps ->
                    let
                        dx = cx - (toFloat x - 100)

                        dy = cy + (toFloat y - 100)
                    in
                        if dx * dx + dy * dy <= 15 * 15 then
                            Just i
                        else
                            ( x, y ) `within` ps

        left e _ = Mouse e

        right p dict = Hover (p `within` Dict.toList dict)

        theAutomaton = Automaton.pure (uncurry (<|)) >>> Drag.automaton Nothing

        update event ( automaton, dict ) =
            case Automaton.step ( event, dict ) automaton of
                ( automaton', Just ( i, MoveBy ( dx, dy ) ) ) ->
                    ( automaton', Dict.update i (Maybe.map (moveBy ( dx, dy ))) dict )

                ( automaton', _ ) ->
                    ( automaton', dict )
    in
        Signal.map
            (collage 200 200 << List.map (\p -> Graphics.Collage.move p (outlined (solid black) (circle 15))) << Dict.values)
            (Signal.map
                snd
                (foldp
                    update
                    ( theAutomaton, Dict.fromList [ ( 1, ( 0, -35 ) ), ( 2, ( 0, 0 ) ), ( 3, ( 0, 35 ) ) ] )
                    (merge (Signal.map left Drag.mouseEvents) (Signal.map right Mouse.position))
                )
            )
