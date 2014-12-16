module DragAndDrop ( MouseEvent(..), mouseEvents, Action(..), track, trackMany, Input(..), automaton ) where
{-| A low- and high-level interface to mouse drag and drop actions.

# The high-level interface
@docs Action, track, trackMany

# The low-level interface
@docs MouseEvent, mouseEvents

# An automaton
@docs Input, automaton
-}

import Mouse
import Maybe (..)
import Maybe
import Automaton
import Automaton (Automaton)
import Signal (..)
import Signal

{-| A type for individual events in a drag and drop sequence. -}
type MouseEvent = StartAt (Int,Int) | MoveFromTo (Int,Int) (Int,Int) | EndAt (Int,Int)

{-| A signal of drag and drop sequence events, fed by `Mouse.isDown`
and `Mouse.position`. A possible history of this signal could be
`StartAt (10,10)`, `MoveFromTo (10,10) (12,10)`, `MoveFromTo (12,10) (13,8)`,
`EndAt (13,8)`, `StartAt (20,15)`, `MoveFromTo (20,15) (22,18)`,
`EndAt (22,18)`, ... -}
mouseEvents : Signal MouseEvent
mouseEvents =
  let assertEqual = always
      f (d,pos') old =
        case (old,d) of
          (Just (StartAt pos),      True) -> Just (MoveFromTo pos pos')
          (Just (StartAt pos),      _)    -> Just (EndAt (assertEqual pos pos'))
          (Just (MoveFromTo _ pos), True) -> Just (MoveFromTo pos pos')
          (Just (MoveFromTo _ pos), _)    -> Just (EndAt (assertEqual pos pos'))
          (_,                       True) -> Just (StartAt pos')
          _                               -> Nothing
      isJust b = case b of
        Just _  -> True
        Nothing -> False
  in withDefault (EndAt (0,0)) <~ (keepIf isJust Nothing <| foldp f Nothing <| Signal.map2 (,) Mouse.isDown Mouse.position)
-- relies on Mouse.isDown and Mouse.position never firing at same time

{-| A type for actions performed on draggable items. -}
type Action = Lift | MoveBy (Int,Int) | Release

{-| Track a single draggable item. The `Bool` and `Signal Bool`
arguments are the initial value and input signal which tell whether
the mouse is (currently) hovering over the draggable item. An example
use:

    hover = Signal.channel False
    
    box = Graphics.Input.hoverable (Signal.send hover)
                                   (putInBox (plainText "drag-and-drop me"))
    
    putInBox e =
      let (sx,sy) = sizeOf e
      in layers [e, collage sx sy [outlined (solid black) (rect (toFloat sx) (toFloat sy))]]
    
    main =
      let moveBy m =
            case m of
              Just (MoveBy (dx,dy)) -> \(x,y) -> (x + toFloat dx, y - toFloat dy)
              _                     -> identity
      in Signal.map (\p -> collage 200 200 [move p (toForm box)])
                    (foldp moveBy (0,0) (track False (Signal.subscribe hover)))
-}
track : Bool -> Signal Bool -> Signal (Maybe Action)
track inside hover =
  let btm b = if b then Just () else Nothing
  in Signal.map (Maybe.map snd) (trackMany (btm inside) (btm <~ hover))

{-| Input type for `automaton`. -}
type Input a = Mouse MouseEvent | Hover (Maybe a)

{-| Track several draggable items. The `Maybe a` and `Signal (Maybe
a)` arguments are the initial value and input signal which tell
whether the mouse is (currently) hovering over a draggable item, and
over which one. An example use (also using `putInBox` from above):

    hover = Signal.channel Nothing
    
    box1 = Graphics.Input.hoverable (Signal.send hover << \h -> if h then Just 1 else Nothing)
                                    (putInBox (plainText "drag-and-drop me"))
    
    box2 = Graphics.Input.hoverable (Signal.send hover << \h -> if h then Just 2 else Nothing)
                                    (putInBox (plainText "and me too"))
    
    main =
      let moveBy m =
            case m of
              Just (1, MoveBy (dx,dy)) -> \((x1,y1), p2) -> ((x1 + toFloat dx, y1 - toFloat dy), p2)
              Just (2, MoveBy (dx,dy)) -> \(p1, (x2,y2)) -> (p1, (x2 + toFloat dx, y2 - toFloat dy))
              _                        -> identity
      in Signal.map (\(p1,p2) -> collage 200 200 [move p1 (toForm box1), move p2 (toForm box2)])
                    (foldp moveBy ((0,15), (0,-15)) (trackMany Nothing (Signal.subscribe hover)))

A more dynamic example can be found in
[Example3.elm](https://github.com/jvoigtlaender/elm-drag-and-drop/blob/master/Example3.elm). -}
trackMany : Maybe a -> Signal (Maybe a) -> Signal (Maybe (a, Action))
trackMany inside hover = Automaton.run (automaton inside) Nothing (merge (Mouse <~ mouseEvents) (Hover <~ hover))

type State a = Outside | Inside a | Picked a (Int,Int) (Maybe a)

{-| An [Automaton](http://package.elm-lang.org/packages/evancz/automaton/latest)
that can be used in specific situations where `track`/`trackMany` are
not applicable. See
[Example4.elm](https://github.com/jvoigtlaender/elm-drag-and-drop/blob/master/Example4.elm).
The automaton is also used internally in the `track` and `trackMany`
functions. -}
automaton : Maybe a -> Automaton (Input a) (Maybe (a, Action))
automaton inside = Automaton.hiddenState (withDefault Outside (Maybe.map Inside inside)) automatonStep

automatonStep : Input a -> State a -> (Maybe (a, Action), State a)
automatonStep event old =
  case (old,event) of
    (Outside,          Hover (Just i))          -> (Nothing, Inside i)
    (Inside _,         Hover mi)                -> (Nothing, withDefault Outside (Maybe.map Inside mi))
    (Inside i,         Mouse (StartAt from))    -> (Just (i, Lift), Picked i from Nothing)
    (Picked i from mj, Mouse (MoveFromTo _ to)) -> let (x,y)   = from
                                                       (x',y') = to
                                                   in (Just (i, MoveBy (x'-x, y'-y)), Picked i to mj)
    (Picked i _ mj,    Mouse (EndAt _))         -> (Just (i, Release), Inside (withDefault i mj))
    (Picked i from _,  Hover mj)                -> (Nothing, Picked i from mj)
    _                                           -> (Nothing, old)
    -- the case (Picked _ _ _, Mouse (StartAt _))  -> ... cannot actually occur
