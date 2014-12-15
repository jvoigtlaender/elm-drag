import DragAndDrop (..)
import Graphics.Input
import Signal (..)
import Signal
import Text (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)

hover = Signal.channel Nothing

box1 = Graphics.Input.hoverable (Signal.send hover << \h -> if h then Just 1 else Nothing) (putInBox (plainText "drag-and-drop me"))

box2 = Graphics.Input.hoverable (Signal.send hover << \h -> if h then Just 2 else Nothing) (putInBox (plainText "and me too"))

putInBox e =
  let (sx,sy) = sizeOf e
  in layers [e, collage sx sy [outlined (solid black) (rect (toFloat sx) (toFloat sy))]]

main =
  let moveBy m =
        case m of
          Just (1, MoveBy (dx,dy)) -> \((x1,y1), p2) -> ((x1 + toFloat dx, y1 - toFloat dy), p2)
          Just (2, MoveBy (dx,dy)) -> \(p1, (x2,y2)) -> (p1, (x2 + toFloat dx, y2 - toFloat dy))
          _                        -> identity
  in Signal.map (\(p1,p2) -> collage 200 200 [move p1 (toForm box1), move p2 (toForm box2)])
                (foldp moveBy ((0,15), (0,-15)) (trackMany Nothing (Signal.subscribe hover)))
