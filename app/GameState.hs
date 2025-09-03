module GameState (
  Vertical,
  Horizontal,
  Symbol (Circle, Cross),
  Selected (Board, EndedOptions),
  TurnState (Ended, Running),
  GameState (turnState, selected, fields),
  initGameState,
  selUp,
  selDown,
  selLeft,
  selRight,
  selConfirm,
)
where

type Vertical = Int
type Horizontal = Int

data Symbol = Circle | Cross
  deriving (Eq, Show)

data Selected = Board (Vertical, Horizontal) | EndedOptions Horizontal
data TurnState = Ended (Maybe Symbol) | Running Symbol
data GameState = GameState
  { fields :: [[Maybe Symbol]]
  , selected :: Selected
  , turnState :: TurnState
  }

initGameState :: GameState
initGameState =
  GameState
    { fields = replicate 3 . replicate 3 $ Nothing
    , selected = Board (0, 0)
    , turnState = Running Circle
    }

selMove :: Vertical -> Horizontal -> GameState -> GameState
selMove v h s =
  let
    height = length . fields $ s
    width = length . head . fields $ s

    newSel = case selected s of
      Board (vo, ho) ->
        Board
          ( (vo + v) `mod` height
          , (ho + h) `mod` width
          )
      EndedOptions ho -> EndedOptions ((ho + h) `mod` 2)
   in
    s{selected = newSel}

selUp :: GameState -> GameState
selUp = selMove (-1) 0

selDown :: GameState -> GameState
selDown = selMove 1 0

selLeft :: GameState -> GameState
selLeft = selMove 0 (-1)

selRight :: GameState -> GameState
selRight = selMove 0 1

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
    claimField Nothing = case turnState s of
      Running f -> Just f
      Ended _ -> undefined

    compNewFields = newFields (fields s) (selected s)
    fieldsLeft = any (elem Nothing) compNewFields

    isWinner p =
      let
        condHorz = any (all (== Just p)) compNewFields
        condVert =
          or
            [ all ((== Just p) . (!! i)) compNewFields
            | i <- [0 .. length (head compNewFields) - 1]
            ]
        condArcoRight =
          all
            (== Just p)
            [ head . head $ compNewFields
            , compNewFields !! 1 !! 1
            , compNewFields !! 2 !! 2
            ]
        condArcoLeft =
          all
            (== Just p)
            [ head compNewFields !! 2
            , compNewFields !! 1 !! 1
            , head (compNewFields !! 2)
            ]
       in
        condHorz || condVert || condArcoRight || condArcoLeft

    newTurnState (Ended p) = Ended p
    newTurnState (Running p)
      | isWinner Circle = Ended (Just Circle)
      | isWinner Cross = Ended (Just Cross)
      | fieldsLeft =
          if compNewFields == fields s
            then Running p
            else case p of
              Circle -> Running Cross
              Cross -> Running Circle
      | otherwise = Ended Nothing

    compNewTurnState = newTurnState (turnState s)
    horzPos = case selected s of
      Board (_, p) -> p `mod` 2
      EndedOptions p -> p
   in
    s
      { fields = compNewFields
      , turnState = compNewTurnState
      , selected = case compNewTurnState of
          Running _ -> selected s
          Ended _ -> EndedOptions horzPos
      }
