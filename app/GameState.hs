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

import Lens.Micro.TH

type Vertical = Int
type Horizontal = Int
type Pos = (Vertical, Horizontal)

data Symbol = Circle | Cross
  deriving (Eq, Show)

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

selUp :: GameState -> GameState
selUp = selMove (-1, 0)

selDown :: GameState -> GameState
selDown = selMove (1, 0)

selLeft :: GameState -> GameState
selLeft = selMove (0, -1)

selRight :: GameState -> GameState
selRight = selMove (0, 1)

selConfirm :: GameState -> GameState
selConfirm s =
  let
    newFields [] _ = []
    newFields rs (EndedOptions _) = rs
    newFields (r : rs) (Board (v, h)) =
      if v == 0
        then newRow r h : newFields rs (Board (v - 1, h))
        else r : newFields rs (Board (v - 1, h))

    newRow [] _ = []
    newRow (f : fs) hsel =
      if hsel == 0
        then claimField f : newRow fs (hsel - 1)
        else f : newRow fs (hsel - 1)

    claimField (Just f) = Just f
    claimField Nothing = case _turnState s of
      Running f -> Just f
      Ended _ -> undefined

    compNewFields = newFields (_fields s) (_selected s)
    fieldsLeft = any (elem Nothing) compNewFields

    boardSize = length . head $ compNewFields
    isWinner p =
      let
        condHorz = any (all (== Just p)) compNewFields
        condVert =
          or
            [ all ((== Just p) . (!! i)) compNewFields
            | i <- [0 .. boardSize - 1]
            ]
        condArcoRight =
          all
            (== Just p)
            [compNewFields !! x !! x | x <- [0 .. boardSize - 1]]
        condArcoLeft =
          all
            (== Just p)
            [compNewFields !! x !! ((boardSize - 1) - x) | x <- [0 .. boardSize - 1]]
       in
        condHorz || condVert || condArcoRight || condArcoLeft

    newTurnState (Ended p) = Ended p
    newTurnState (Running p)
      | isWinner Circle = Ended (Just Circle)
      | isWinner Cross = Ended (Just Cross)
      | fieldsLeft =
          if compNewFields == _fields s
            then Running p
            else case p of
              Circle -> Running Cross
              Cross -> Running Circle
      | otherwise = Ended Nothing

    compNewTurnState = newTurnState (_turnState s)
    horzPos = case _selected s of
      Board (_, p) -> p `mod` 2
      EndedOptions p -> p
   in
    s
      { _fields = compNewFields
      , _turnState = compNewTurnState
      , _selected = case compNewTurnState of
          Running _ -> _selected s
          Ended _ -> EndedOptions horzPos
      }

selAndConfirm :: Pos -> GameState -> GameState
selAndConfirm p s =
  let
    newSel = case _selected s of
      Board _ -> Board p
      EndedOptions h -> EndedOptions h
   in
    selConfirm s{_selected = newSel}
