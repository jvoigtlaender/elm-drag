import DragAndDrop (..)
import Graphics.Input
import Signal (..)
import Signal
import Text (plainText)
import Graphics.Element (..)
import Graphics.Collage (..)
import Graphics.Collage
import Color (..)

hover = Signal.channel Nothing

box1 = Graphics.Input.hoverable (Signal.send hover << \h -> if h then Just 1 else Nothing)
                                (putInBox (plainText "drag-and-drop me"))

box2 = Graphics.Input.hoverable (Signal.send hover << \h -> if h then Just 2 else Nothing)
                                (putInBox (plainText "and me too"))

putInBox e =
  let (sx,sy) = sizeOf e
  in layers [e, collage sx sy [outlined (solid black) (rect (toFloat sx) (toFloat sy))]]

moveBy (dx,dy) (x,y) = (x + toFloat dx, y - toFloat dy)

main =
  let update m =
        case m of
          Just (1, MoveBy (dx,dy)) -> \(p1,p2) -> (moveBy (dx,dy) p1, p2)
          Just (2, MoveBy (dx,dy)) -> \(p1,p2) -> (p1, moveBy (dx,dy) p2)
          _                        -> identity
  in Signal.map (\(p1,p2) -> collage 200 200 [Graphics.Collage.move p1 (toForm box1),
                                              Graphics.Collage.move p2 (toForm box2)])
                (foldp update ((0,15), (0,-15)) (trackMany Nothing (Signal.subscribe hover)))
