import DragAndDrop (..)
import Graphics.Input
import Signal (..)
import Signal
import Text (plainText)
import Graphics.Element (..)
import Graphics.Collage (..)
import Graphics.Collage
import Color (..)

hover = Signal.channel False

box = Graphics.Input.hoverable (Signal.send hover)
                               (putInBox (plainText "drag-and-drop me"))

putInBox e =
  let (sx,sy) = sizeOf e
  in layers [e, collage sx sy [outlined (solid black) (rect (toFloat sx) (toFloat sy))]]

moveBy (dx,dy) (x,y) = (x + toFloat dx, y - toFloat dy)

main =
  let update m =
        case m of
          Just (MoveBy (dx,dy)) -> moveBy (dx,dy)
          _                     -> identity
  in Signal.map (\p -> collage 200 200 [Graphics.Collage.move p (toForm box)])
                (foldp update (0,0) (track False (Signal.subscribe hover)))
