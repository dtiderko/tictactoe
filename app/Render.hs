module Render where

import Data.List (intersperse)

import Brick
import Brick.Widgets.Center
import GameState

myDraw :: GameState -> [Widget ()]
myDraw s =
  [ center $
      foldl
        (<=>)
        emptyWidget
        ( map
            hCenter
            [ title
            , padTopBottom 1 $ renderGrid (_fields s) (_selected s)
            , footer (_turnState s)
            , endedOptions (_turnState s) (_selected s)
            ]
        )
  ]

title :: Widget ()
title = vBox . map str $ title_text
 where
  title_text =
    [ "▀█▀ █ ▄▀▀ ▀█▀ ▄▀▄ ▄▀▀ ▀█▀ ▄▀▄ ██▀"
    , " █  █ ▀▄▄  █  █▀█ ▀▄▄  █  ▀▄▀ █▄▄"
    ]

renderGrid :: [[Maybe Symbol]] -> Selected -> Widget ()
renderGrid [] _ = emptyWidget
renderGrid g sel =
  let
    rows = renderRows g sel
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

renderRows :: [[Maybe Symbol]] -> Selected -> [[Widget ()]]
renderRows [] _ = []
renderRows (r : rs) (EndedOptions _) = renderRow r Nothing : renderRows rs (EndedOptions 0)
renderRows (r : rs) (Board (v, h)) =
  let
    current = renderRow r (if v == 0 then Just h else Nothing)
    rest = renderRows rs (Board (v - 1, h))
   in
    current : rest

renderRow :: [Maybe Symbol] -> Maybe Horizontal -> [Widget ()]
renderRow [] _ = []
renderRow (f : fs) Nothing = renderField f False : renderRow fs Nothing
renderRow (f : fs) (Just h) = renderField f (h == 0) : renderRow fs (Just (h - 1))

renderField :: Maybe Symbol -> Bool -> Widget ()
renderField f sel =
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
    attr $ str inner

footer :: TurnState -> Widget ()
footer (Running p) = str $ "It is your turn " ++ show p ++ "!"
footer (Ended (Just p)) = str $ show p ++ " won!"
footer (Ended Nothing) = str "Draw!"

endedOptions :: TurnState -> Selected -> Widget ()
endedOptions (Running _) _ = emptyWidget
endedOptions (Ended _) (Board _) = undefined
endedOptions (Ended _) (EndedOptions p) =
  let
    highIfSel ipos =
      if ipos == p
        then withAttr (attrName "highlight")
        else id
   in
    padTop (Pad 1) $
      hBox
        [ padRight (Pad 8) $ highIfSel 0 $ str "<Restart>"
        , highIfSel 1 $ str "<Quit>"
        ]
