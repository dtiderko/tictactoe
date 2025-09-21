{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module GameState (
  Horizontal,
  Vertical,
  Pos,
  Symbol (Circle, Cross),
  Selected (Board, EndedOptions),
  TurnState (Ended, Running),
  GameState (_turnState, _selected, _fields),
  initGameState,
  selUp,
  selDown,
  selLeft,
  selRight,
  selConfirm,
  selAndConfirm,
  -- lenses
  selected,
  fields,
  turnState,
)
where

import Data.List (transpose)
import Lens.Micro
import Lens.Micro.TH

type Vertical = Int
type Horizontal = Int
type Pos = (Vertical, Horizontal)

data Symbol = Circle | Cross deriving (Eq, Show)
data Selected = Board Pos | EndedOptions Horizontal
data TurnState = Ended (Maybe Symbol) | Running Symbol
data GameState = GameState
  { _fields :: [[Maybe Symbol]]
  , _selected :: Selected
  , _turnState :: TurnState
  }
makeLenses ''GameState

initGameState :: GameState
initGameState =
  GameState
    { _fields = replicate 3 . replicate 3 $ Nothing
    , _selected = Board (0, 0)
    , _turnState = Running Circle
    }

selMove :: Pos -> GameState -> GameState
selMove (v, h) s =
  let
    height = length . _fields $ s
    width = length . head . _fields $ s

    newSel = case _selected s of
      Board (vo, ho) ->
        Board
          ( (vo + v) `mod` height
          , (ho + h) `mod` width
          )
      EndedOptions ho -> EndedOptions ((ho + h) `mod` 2)
   in
    s{_selected = newSel}

selUp, selDown, selLeft, selRight :: GameState -> GameState
selUp = selMove (-1, 0)
selDown = selMove (1, 0)
selLeft = selMove (0, -1)
selRight = selMove (0, 1)

isWinner :: Symbol -> [[Maybe Symbol]] -> Bool
isWinner p board =
  let
    boardSize = length (head board)
    condDiag f = all (== Just p) [board !! i !! f i | i <- [0 .. boardSize - 1]]
    condLine = any (all (== Just p))
   in
    condLine board
      || condLine (transpose board)
      || condDiag id
      || condDiag (boardSize - 1 -)

claimOnBoard :: Pos -> Symbol -> [[Maybe Symbol]] -> [[Maybe Symbol]]
claimOnBoard (v, h) curTurn =
  over
    (ix v . ix h)
    ( \case
        Nothing -> Just curTurn
        Just x -> Just x
    )

selConfirm :: GameState -> GameState
selConfirm s@GameState{_selected = sel, _fields = fs, _turnState = ts} =
  let
    fs' = case (sel, ts) of
      (Board bsel, Running curTurn) -> claimOnBoard bsel curTurn fs
      (Board _, Ended _) -> undefined
      (EndedOptions _, Running _) -> undefined
      (_, _) -> fs

    ts' = case ts of
      Ended p -> Ended p
      Running p
        | isWinner Circle fs' -> Ended (Just Circle)
        | isWinner Cross fs' -> Ended (Just Cross)
        | fieldsLeft && fs' == fs -> Running p
        | fieldsLeft -> Running (if p == Circle then Cross else Circle)
        | otherwise -> Ended Nothing
     where
      fieldsLeft = any (elem Nothing) fs'

    sel' = case ts' of
      Running _ -> sel
      Ended _ -> EndedOptions horzPos
     where
      horzPos = case sel of
        Board (_, p) -> p `mod` 2
        EndedOptions p -> p
   in
    s
      { _fields = fs'
      , _turnState = ts'
      , _selected = sel'
      }

selAndConfirm :: Pos -> GameState -> GameState
selAndConfirm p s =
  let
    newSel = case _selected s of
      Board _ -> Board p
      EndedOptions h -> EndedOptions h
   in
    selConfirm s{_selected = newSel}
