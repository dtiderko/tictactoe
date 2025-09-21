module Main where

import Brick

import Graphics.Vty
import Lens.Micro.Mtl

import Render

import GameState

main :: IO GameState
main = do
  let app =
        App
          { appChooseCursor = const . const Nothing
          , appStartEvent = return ()
          , appAttrMap = myAttrMap
          , appHandleEvent = myHandleEvent
          , appDraw = myDraw
          }

  defaultMain app initGameState

myAttrMap :: GameState -> AttrMap
myAttrMap = const $ attrMap defAttr [(attrName "highlight", black `on` white)]

myHandleEvent :: BrickEvent () e -> EventM () GameState ()
myHandleEvent (VtyEvent e) = case e of
  EvKey (KChar 'q') [] -> halt
  EvKey (KChar 'c') [MCtrl] -> halt
  EvKey KUp [] -> modify selUp
  EvKey KDown [] -> modify selDown
  EvKey KLeft [] -> modify selLeft
  EvKey KRight [] -> modify selRight
  EvKey KEnter [] -> onSelect
  EvKey (KChar ' ') [] -> onSelect
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
myHandleEvent _ = undefined
