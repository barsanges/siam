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
  , idxes
  , Board
  , initialBoard
  , numberOut
  , lookupCell
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

-- | Ensemble des indices des cases : chaque liste correspond à un
-- rang, de gauche à droite et de haut en bas.
idxes :: [[Idx]]
idxes = [ [ Idx (i + 5 * j) | i <- [0..4] ]
        | j <- [0..4]
        ]

-- | Le plateau de jeu.
data Board = Board { content_ :: IM.IntMap Pawn
                   , elephantsOut_ :: Int
                   , rhinosOut_ :: Int
                   }
  deriving Show

-- | Le plateau de jeu en début de partie.
initialBoard :: Board
initialBoard = Board { content_ = IM.fromList [ (11, Rock)
                                              , (12, Rock)
                                              , (13, Rock)
                                              ]
                     , elephantsOut_ = 5
                     , rhinosOut_ = 5
                     }

-- | Renvoie le nombre d'éléphants ou de rhinocéros en dehors du plateau.
numberOut :: Board -> Faction -> Int
numberOut b Elephant = elephantsOut_ b
numberOut b Rhino = rhinosOut_ b

-- | Renvoie le contenu d'une cellule.
lookupCell :: Idx -> Board -> Maybe Pawn
lookupCell (Idx idx) b = IM.lookup idx (content_ b)
