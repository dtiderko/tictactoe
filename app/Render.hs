module Render where

import Data.List (intersperse)
import Graphics.Vty

import GameState

render :: DisplayRegion -> GameState -> Picture
render dr s =
  let
    content =
      [ title
      , pad 0 1 0 1 $ renderGrid (fields s) (selected s)
      , footer (turnState s)
      ]

    contentWidth = maximum . map imageWidth $ content
    toPad l = (contentWidth - imageWidth l) `div` 2
    paddedContent = map (\l -> pad (toPad l) 0 0 0 l) content
    image = vertCat paddedContent

    leftPad = (regionWidth dr - imageWidth image) `div` 2
    topPad = (regionHeight dr - imageHeight image) `div` 2
   in
    if leftPad < 0 || topPad < 0
      then picForImage $ string defAttr "Window too small!"
      else picForImage $ pad leftPad topPad 0 0 image

title :: Image
title = vertCat . map (string defAttr) $ title_text
 where
  title_text =
    [ "▀█▀ █ ▄▀▀ ▀█▀ ▄▀▄ ▄▀▀ ▀█▀ ▄▀▄ ██▀"
    , " █  █ ▀▄▄  █  █▀█ ▀▄▄  █  ▀▄▀ █▄▄"
    ]

renderGrid :: [[Maybe Symbol]] -> Maybe (Vertical, Horizontal) -> Image
renderGrid [] _ = emptyImage
renderGrid g sel =
  let
    rows = renderRows g sel
    sep = string defAttr "┃"
    rowsWithSep = map (\r -> sep : intersperse sep r ++ [sep]) rows
    gameRows = map horizCat rowsWithSep

    width = length . head $ g
    lineWith l c r = string defAttr (l ++ drop 1 (concat (replicate width c) ++ r))
    top = lineWith "┏" "┳━━━" "┓"
    mid = lineWith "┣" "╋━━━" "┫"
    bottom = lineWith "┗" "┻━━━" "┛"

    gameRowsWithSep = top : intersperse mid gameRows ++ [bottom]
   in
    vertCat gameRowsWithSep

renderRows :: [[Maybe Symbol]] -> Maybe (Vertical, Horizontal) -> [[Image]]
renderRows [] _ = []
renderRows (r : rs) Nothing = renderRow r Nothing : renderRows rs Nothing
renderRows (r : rs) (Just (v, h)) =
  let
    current = renderRow r (if v == 0 then Just h else Nothing)
    rest = renderRows rs (Just (v - 1, h))
   in
    current : rest

renderRow :: [Maybe Symbol] -> Maybe Horizontal -> [Image]
renderRow [] _ = []
renderRow (f : fs) Nothing = renderField f False : renderRow fs Nothing
renderRow (f : fs) (Just h) = renderField f (h == 0) : renderRow fs (Just (h - 1))

renderField :: Maybe Symbol -> Bool -> Image
renderField f sel =
  let
    attr =
      if sel
        then defAttr `withForeColor` black `withBackColor` white
        else defAttr
    txt = case f of
      Nothing -> "   "
      (Just Circle) -> " O "
      (Just Cross) -> " X "
   in
    string attr txt

footer :: TurnState -> Image
footer (Running p) = string defAttr $ "It is your turn " ++ show p ++ "!"
footer (Ended (Just p)) = string defAttr $ show p ++ " won!"
footer (Ended Nothing) = string defAttr "Draw!"
