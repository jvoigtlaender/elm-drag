import Dict
import Automaton
import Automaton ( (>>>) )
import DragAndDrop
import DragAndDrop ( Input(..), Action(..) )
import Mouse
import Signal (..)
import Signal
import Graphics.Collage (..)
import Color (..)
import List

main =
  let (x,y) `within` ps =
        case ps of
          []              -> Nothing
          (i,(cx,cy))::ps -> let dx = cx - (toFloat x - 100)
                                 dy = cy + (toFloat y - 100)
                             in if dx*dx + dy*dy <= 15*15 then Just i else (x,y) `within` ps
      left e _ = Mouse e
      right p dict = Hover (p `within` Dict.toList dict)
      theAutomaton = Automaton.pure (uncurry (<|)) >>> DragAndDrop.automaton Nothing
      act event (automaton, dict) =
        case Automaton.step (event, dict) automaton of
          (automaton', Just (i, MoveBy (dx,dy))) -> (automaton', Dict.update i (\(Just (x,y)) -> Just (x + toFloat dx, y - toFloat dy)) dict)
          (automaton', _)                        -> (automaton', dict)
  in Signal.map (collage 200 200 << List.map (\p -> move p (outlined (solid black) (circle 15))) << Dict.values)
                (snd <~ (foldp act (theAutomaton, Dict.fromList [(1,(0,-35)), (2,(0,0)), (3,(0,35))])
                                   (merge (left <~ DragAndDrop.mouseEvents) (right <~ Mouse.position))))
