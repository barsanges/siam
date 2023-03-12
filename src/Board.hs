{- |
   Module      : Board
   Copyright   : Copyright (C) 2023 barsanges

Représentation d'un plateau de Siam.
-}

module Board
  ( Faction(..)
  , Orientation(..)
  , Pawn(..)
  , Idx
  , Board
  ) where

import qualified Data.IntMap as IM

-- | Une des deux factions en présence.
data Faction = Elephant | Rhino
  deriving (Eq, Show)

-- | L'orientation d'une pièce.
data Orientation = North
                 | West
                 | South
                 | East
  deriving (Eq, Show)

-- | Un pion sur le plateau.
data Pawn = Animal Faction Orientation
          | Rock
  deriving (Eq, Show)

-- | Identifiant d'une case du plateau de jeu.
data Idx = Idx Int
  deriving (Eq, Show)

-- | Le plateau de jeu.
data Board = Board { content_ :: IM.IntMap (Maybe Pawn)
                   , elephantsOut_ :: Int
                   , rhinosOut_ :: Int
                   }
  deriving Show
