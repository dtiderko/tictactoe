module Main where

import Brick

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Events
import GameState
import Graphics.Vty
import Render

main :: IO GameState
main = do
  let app =
        App
          { appChooseCursor = const . const Nothing
          , appStartEvent = enableMouseSupport
          , appAttrMap = myAttrMap
          , appHandleEvent = myHandleEvent
          , appDraw = myDraw
          }

  defaultMain app initGameState

enableMouseSupport :: EventM Clickable GameState ()
enableMouseSupport = do
  vty <- getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $
      setMode output Mouse True

myAttrMap :: GameState -> AttrMap
myAttrMap = const $ attrMap defAttr [(attrName "highlight", black `on` white)]
