module Events where

import Brick
import Lens.Micro.Mtl

import GameState
import Graphics.Vty (Button (BLeft))
import qualified Graphics.Vty as V

data Clickable = Restart | Quit | Cell Pos
  deriving (Ord, Eq)

myHandleEvent :: BrickEvent Clickable e -> EventM Clickable GameState ()
myHandleEvent MouseDown{} = return ()
myHandleEvent (MouseUp b (Just BLeft) _) = case b of
  Restart -> modify $ const initGameState
  Quit -> halt
  Cell p -> modify $ selAndConfirm p
myHandleEvent (VtyEvent e) = case e of
  V.EvKey (V.KChar 'q') [] -> halt
  V.EvKey (V.KChar 'c') [V.MCtrl] -> halt
  V.EvKey V.KUp [] -> modify selUp
  V.EvKey V.KDown [] -> modify selDown
  V.EvKey V.KLeft [] -> modify selLeft
  V.EvKey V.KRight [] -> modify selRight
  V.EvKey V.KEnter [] -> onSelect
  V.EvKey (V.KChar ' ') [] -> onSelect
  _ -> return ()
 where
  onSelect = do
    sel <- use selected
    case sel of
      Board _ -> modify selConfirm
      EndedOptions p ->
        if p == 0
          then modify $ const initGameState
          else halt
myHandleEvent _ = return ()
