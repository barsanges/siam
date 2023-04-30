{- |
   Module      : Board
   Copyright   : Copyright (C) 2023 barsanges

Représentation d'un plateau de Siam.
-}

module Board
  ( Faction(..)
  , Orientation(..)
  , parseOrientation
  , Pawn(..)
  , Idx
  , mkIdx
  , parseIdx
  , edge
  , neighbors
  , Board
  , initialBoard
  , numberOut
  , lookupCell
  ) where

import Data.List ( elemIndex )
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Utils

-- | Une des deux factions en présence.
data Faction = Elephant | Rhino
  deriving (Eq, Show)

-- | L'orientation d'une pièce.
data Orientation = North
                 | West
                 | South
                 | East
  deriving (Eq, Show)

-- | Renvoie une orientation à partir d'une chaîne de caractères.
parseOrientation :: String -> Maybe Orientation
parseOrientation x = case sanitizeStr x of
  "n" -> Just North
  "w" -> Just West
  "s" -> Just South
  "e" -> Just East
  _ -> Nothing

-- | Un pion sur le plateau.
data Pawn = Animal Faction Orientation
          | Rock
  deriving (Eq, Show)

-- | Identifiant d'une case du plateau de jeu.
data Idx = Idx Int
  deriving (Eq, Ord)

instance Show Idx where
  show idx = [c, r]
    where
      (i, j) = toPair idx
      r = ['5', '4', '3', '2', '1'] !! i
      c = ['a', 'b', 'c', 'd', 'e'] !! j

-- | Transforme un indice en un indice de ligne et un indice de colonne.
toPair :: Idx -> (Int, Int)
toPair (Idx idx) = (idx `div` 5, idx `mod` 5)

-- | Construit un indice à partir d'un indice de ligne et d'un indice de
-- colonne.
unsafeFromPair :: Int -> Int -> Idx
unsafeFromPair i j = Idx (j + 5 * i)

-- | Renvoie un indice à partir d'un entier.
mkIdx :: Int -> Maybe Idx
mkIdx i = if (0 <= i) && (i < 25)
          then Just (Idx i)
          else Nothing

-- | Renvoie un indice à partir d'une chaîne de caractères.
parseIdx :: String -> Maybe Idx
parseIdx str = go (sanitizeStr str)
  where
    go (x:y:[]) = do
      i <- y `elemIndex` ['5', '4', '3', '2', '1']
      j <- x `elemIndex` ['a', 'b', 'c', 'd', 'e']
      return (unsafeFromPair i j)
    go _ = Nothing

-- | Renvoie les indices des cases en bordure du plateau.
edge :: S.Set Idx
edge = S.fromList (fmap Idx [0, 1, 2, 3, 4, 5, 9, 10, 14,
                             15, 19, 20, 21, 22, 23, 24])

-- | Renvoie les indices des cases adjacentes à la case donnée.
neighbors :: Idx -> S.Set Idx
neighbors idx = S.fromList [unsafeFromPair i' j' | (i', j') <- [ (i - 1, j)
                                                               , (i, j + 1)
                                                               , (i + 1, j)
                                                               , (i, j - 1)]
                                                 , 0 <= i'
                                                 , i' < 5
                                                 , 0 <= j'
                                                 , j' < 5]
  where
    (i, j) = toPair idx

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
