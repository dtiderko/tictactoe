module Main where

import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)

import Control.Concurrent
import Render

import GameState

main :: IO ()
main = do
  vty <- mkVty defaultConfig
  setWindowTitle vty "TicTacToe"

  gameLoop vty initGameState

gameLoop :: Vty -> GameState -> IO ()
gameLoop vty s = do
  dr <- displayBounds . outputIface $ vty
  update vty $ render dr s

  e <- nextEvent vty
  case e of
    EvKey (KChar 'q') [] -> shutdown vty
    EvKey (KChar 'c') [MCtrl] -> shutdown vty
    EvKey KUp [] -> gameLoop vty $ selUp s
    EvKey KDown [] -> gameLoop vty $ selDown s
    EvKey KLeft [] -> gameLoop vty $ selLeft s
    EvKey KRight [] -> gameLoop vty $ selRight s
    EvKey KEnter [] -> gameLoop vty $ nextTurn s
    EvKey (KChar ' ') [] -> gameLoop vty $ nextTurn s
    _ -> do
      update vty . picForImage . string defAttr $ show e
      threadDelay 100000
      gameLoop vty s
