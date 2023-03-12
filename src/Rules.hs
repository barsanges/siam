{- |
   Module      : Rules
   Copyright   : Copyright (C) 2023 barsanges

Implémentation des règles de Siam.
-}

module Rules
  ( Game(..)
  , Move
  ) where

import Board

-- | La partie, soit en cours soit terminée.
data Game = Ongoing Faction Board
          | Ended Faction Board
  deriving Show

-- | Un coup, non-nécessairement admissible.
data Move = Enter Idx Orientation
          | Movement Idx Idx Orientation
          | Exit Idx
  deriving Show
