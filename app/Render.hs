module Render where

import Data.List (intersperse)

import Brick
import Brick.Widgets.Center

import Events
import GameState

myDraw :: GameState -> [Widget Clickable]
myDraw s =
  [ center . vBox $
      map
        hCenter
        [ title
        , padTopBottom 1 $ renderGrid (_fields s) (_selected s)
        , footer (_turnState s)
        , endedOptions (_turnState s) (_selected s)
        ]
  ]

title :: Widget Clickable
title = vBox . map str $ title_text
 where
  title_text =
    [ "▀█▀ █ ▄▀▀ ▀█▀ ▄▀▄ ▄▀▀ ▀█▀ ▄▀▄ ██▀"
    , " █  █ ▀▄▄  █  █▀█ ▀▄▄  █  ▀▄▀ █▄▄"
    ]

renderGrid :: [[Maybe Symbol]] -> Selected -> Widget Clickable
renderGrid [] _ = emptyWidget
renderGrid g sel =
  let
    rows = renderRows g sel 0
    sep = str "┃"
    rowsWithSep = map (\r -> sep : intersperse sep r ++ [sep]) rows
    gameRows = map hBox rowsWithSep

    width = length . head $ g
    lineWith l c r = str (l ++ drop 1 (concat (replicate width c) ++ r))
    top = lineWith "┏" "┳━━━" "┓"
    mid = lineWith "┣" "╋━━━" "┫"
    bottom = lineWith "┗" "┻━━━" "┛"

    gameRowsWithSep = top : intersperse mid gameRows ++ [bottom]
   in
    vBox gameRowsWithSep

renderRows :: [[Maybe Symbol]] -> Selected -> Vertical -> [[Widget Clickable]]
renderRows rs sel _ = zipWith (\i -> renderRow (selAtRow i) i) [0 ..] rs
 where
  selAtRow i = case sel of
    Board (v, h) | i == v -> Just h
    _ -> Nothing

renderRow :: Maybe Horizontal -> Vertical -> [Maybe Symbol] -> [Widget Clickable]
renderRow sel v = zipWith (\i f -> renderField f (Just i == sel) v i) [0 ..]

renderField :: Maybe Symbol -> Bool -> Vertical -> Horizontal -> Widget Clickable
renderField f sel vert horz =
  let
    attr =
      if sel
        then withAttr (attrName "highlight")
        else id
    inner = case f of
      Nothing -> "   "
      (Just Circle) -> " O "
      (Just Cross) -> " X "
   in
    attr $ clickable (Cell (vert, horz)) $ str inner

footer :: TurnState -> Widget Clickable
footer (Running p) = str $ "It is your turn " ++ show p ++ "!"
footer (Ended (Just p)) = str $ show p ++ " won!"
footer (Ended Nothing) = str "Draw!"

endedOptions :: TurnState -> Selected -> Widget Clickable
endedOptions (Running _) _ = emptyWidget
endedOptions (Ended _) (Board _) = undefined
endedOptions (Ended _) (EndedOptions p) =
  let
    highIfSel ipos =
      if ipos == p
        then withAttr (attrName "highlight")
        else id

    restart =
      padRight (Pad 8) $
        highIfSel 0 $
          clickable Restart $
            str "<Restart>"
    quit = highIfSel 1 $ clickable Quit $ str "<Quit>"
   in
    padTop (Pad 1) $
      hBox
        [ restart
        , quit
        ]
