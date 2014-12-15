import DragAndDrop (..)
import Graphics.Input
import Signal (..)
import Signal
import Text (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)

hover = Signal.channel False

box = Graphics.Input.hoverable (Signal.send hover) (putInBox (plainText "drag-and-drop me"))

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
