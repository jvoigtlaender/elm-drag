-- this example runs at http://share-elm.com/sprout/54193b54e4b0d19703e9773b

import Dict
import DragAndDrop (..)
import Graphics.Input
import Signal (..)
import Signal
import Text (plainText)
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import List

add = Signal.channel ()

button = Graphics.Input.button (Signal.send add ()) "add a draggable box"

hover = Signal.channel Nothing

makeBox i = Graphics.Input.hoverable (Signal.send hover << \h -> if h then Just i else Nothing) (putInBox (plainText (toString i)))

putInBox e =
  let (sx,sy) = sizeOf e
  in layers [e, collage sx sy [outlined (solid black) (rect (toFloat sx) (toFloat sy))]]

type Event = Add Int | Track (Maybe (Int, Action))

main =
  let act e =
        case e of
          Add i                            -> Dict.insert i ((0,0), color yellow (makeBox i))
          Track (Just (i, Lift))           -> Dict.update i (\(Just (p,b)) -> Just (p, color orange b))
          Track (Just (i, MoveBy (dx,dy))) -> Dict.update i (\(Just ((x,y), b)) -> Just ((x + toFloat dx, y - toFloat dy), b))
          Track (Just (i, Release))        -> Dict.update i (\(Just (p,b)) -> Just (p, color yellow b))
          _                                -> identity
  in Signal.map (\dict -> flow down [ button
                                    , collage 200 200 (List.map (\(p,b) -> move p (toForm b)) (Dict.values dict))
                                    ])
                (foldp act Dict.empty (merge (Add <~ foldp (\_ t -> t + 1) 0 (Signal.subscribe add)) (Track <~ trackMany Nothing (Signal.subscribe hover))))
