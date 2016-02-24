module Drag (MouseEvent(..), mouseEvents, Action(..), track, trackMany, Input(..), automaton) where

{-| A low- and high-level interface to mouse drag actions.

# The high-level interface
@docs Action, track, trackMany

# The low-level interface
@docs MouseEvent, mouseEvents

# An automaton
@docs Input, automaton
-}

import Mouse
import Maybe exposing (withDefault)
import Automaton exposing (Automaton)
import Signal exposing (foldp, merge)


{-| A type for individual events in a drag sequence.
-}
type MouseEvent
    = StartAt ( Int, Int )
    | MoveFromTo ( Int, Int ) ( Int, Int )
    | EndAt ( Int, Int )


{-| A signal of drag sequence events, fed by `Mouse.isDown`
and `Mouse.position`. A possible history of this signal could be
`StartAt (10,10)`, `MoveFromTo (10,10) (12,10)`, `MoveFromTo (12,10) (13,8)`,
`EndAt (13,8)`, `StartAt (20,15)`, `MoveFromTo (20,15) (22,18)`,
`EndAt (22,18)`, ...
-}
mouseEvents : Signal MouseEvent
mouseEvents =
    let
        assertEqual = always

        f ( d, pos' ) old =
            case ( old, d ) of
                ( Just (StartAt pos), True ) ->
                    Just (MoveFromTo pos pos')

                ( Just (StartAt pos), _ ) ->
                    Just (EndAt (assertEqual pos pos'))

                ( Just (MoveFromTo _ pos), True ) ->
                    Just (MoveFromTo pos pos')

                ( Just (MoveFromTo _ pos), _ ) ->
                    Just (EndAt (assertEqual pos pos'))

                ( _, True ) ->
                    Just (StartAt pos')

                _ ->
                    Nothing
    in
        Signal.filterMap identity (EndAt ( 0, 0 ))
            <| foldp f Nothing
            <| Signal.map2 (,) Mouse.isDown Mouse.position
-- relies on Mouse.isDown and Mouse.position never firing at same time


{-| A type for actions performed on draggable items.
-}
type Action
    = Lift
    | MoveBy ( Int, Int )
    | Release


{-| Track a single draggable item. The `Bool` and `Signal Bool`
arguments are the initial value and input signal which tell whether
the mouse is (currently) hovering over the draggable item. An example
use
([Example1.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example1.elm) -
[demo](https://jvoigtlaender.github.io/elm-drag/Example1.html)):

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
-}
track : Bool -> Signal Bool -> Signal (Maybe Action)
track inside hover =
    let
        btm b =
            if b then
                Just ()
            else
                Nothing
    in
        Signal.map (Maybe.map snd) (trackMany (btm inside) (Signal.map btm hover))


{-| Input type for [`automaton`](#automaton).
-}
type Input a
    = Mouse MouseEvent
    | Hover (Maybe a)


{-| Track several draggable items. The `Maybe a` and `Signal (Maybe
a)` arguments are the initial value and input signal which tell
whether the mouse is (currently) hovering over a draggable item, and
over which one. An example use
([Example2.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example2.elm) -
[demo](https://jvoigtlaender.github.io/elm-drag/Example2.html),
also using `putInBox` and `moveBy` from above):

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

A more dynamic example can be found in
[Example3.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example3.elm)
([demo](https://jvoigtlaender.github.io/elm-drag/Example3.html)), or using
[`start-app`](http://package.elm-lang.org/packages/evancz/start-app/latest), in
[Example3b.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example3b.elm)
([demo](https://jvoigtlaender.github.io/elm-drag/Example3b.html)).
-}
trackMany : Maybe a -> Signal (Maybe a) -> Signal (Maybe ( a, Action ))
trackMany inside hover =
    Automaton.run (automaton inside) Nothing (merge (Signal.map Mouse mouseEvents) (Signal.map Hover hover))


type State a
    = Outside
    | Inside a
    | Picked a ( Int, Int ) (Maybe a)


{-| An [Automaton](http://package.elm-lang.org/packages/evancz/automaton/latest)
that can be used in specific situations where [`track`](#track)/[`trackMany`](#trackMany) are
not applicable. See
[Example4.elm](https://github.com/jvoigtlaender/elm-drag/blob/master/Example4.elm)
([demo](https://jvoigtlaender.github.io/elm-drag/Example4.html)).
The automaton is also used internally in the [`track`](#track) and [`trackMany`](#trackMany)
functions.
-}
automaton : Maybe a -> Automaton (Input a) (Maybe ( a, Action ))
automaton inside =
    Automaton.hiddenState (withDefault Outside (Maybe.map Inside inside)) automatonStep


automatonStep : Input a -> State a -> ( Maybe ( a, Action ), State a )
automatonStep event old =
    case ( old, event ) of
        ( Outside, Hover (Just i) ) ->
            ( Nothing, Inside i )

        ( Inside _, Hover (Just i) ) ->
            ( Nothing, Inside i )

        ( Inside _, Hover _ ) ->
            ( Nothing, Outside )

        ( Inside i, Mouse (StartAt from) ) ->
            ( Just ( i, Lift ), Picked i from Nothing )

        ( Picked i from mj, Mouse (MoveFromTo _ to) ) ->
            let
                ( x, y ) = from

                ( x', y' ) = to
            in
                ( Just ( i, MoveBy ( x' - x, y' - y ) ), Picked i to mj )

        ( Picked i _ (Just j), Mouse (EndAt _) ) ->
            ( Just ( i, Release ), Inside j )

        ( Picked i _ _, Mouse (EndAt _) ) ->
            ( Just ( i, Release ), Inside i )

        ( Picked i from _, Hover mj ) ->
            ( Nothing, Picked i from mj )

        _ ->
            ( Nothing, old )

        -- the case ( Picked _ _ _, Mouse (StartAt _) ) -> ... cannot actually occur
