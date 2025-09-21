module Main where

import Brick

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Events
import GameState
import Graphics.Vty
import Render
import System.IO

main :: IO ()
main = do
  let app =
        App
          { appChooseCursor = const . const Nothing
          , appStartEvent = enableMouseSupport
          , appAttrMap = myAttrMap
          , appHandleEvent = myHandleEvent
          , appDraw = myDraw
          }

  _ <- enableMouse
  _ <- defaultMain app initGameState
  disableMouse

enableMouse :: IO ()
enableMouse = do
  putStr "\ESC[?1000h" -- basic mouse enable
  putStr "\ESC[?1002h" -- button+motion enable
  putStr "\ESC[?1006h" -- SGR mode enable
  hFlush stdout

disableMouse :: IO ()
disableMouse = do
  putStr "\ESC[?1000l" -- basic mouse disable
  putStr "\ESC[?1002l" -- button+motion disable
  putStr "\ESC[?1006l" -- SGR mode disable
  hFlush stdout

enableMouseSupport :: EventM Clickable GameState ()
enableMouseSupport = do
  vty <- getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $
      setMode output Mouse True

myAttrMap :: GameState -> AttrMap
myAttrMap = const $ attrMap defAttr [(attrName "highlight", black `on` white)]
