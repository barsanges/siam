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
  , line
  , Board
  , initialBoard
  , numberOut
  , lookupCell
  , pop
  , put
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

-- | Renvoie les indices des cases formant une ligne dans la direction donnée à
-- partir de la case donnée. La première case (celle fournie dans l'appel de la
-- fonction) n'est pas incluse dans le retour de la fonction.
line :: Idx -> Orientation -> [Idx]
line idx dir = case dir of
  North -> [unsafeFromPair i' j | i' <- [(i-1), (i-2)..0]]
  West -> [unsafeFromPair i j' | j' <- [(j-1), (j-2)..0]]
  South -> [unsafeFromPair i' j |  i' <- [(i+1)..4]]
  East -> [unsafeFromPair i j' | j' <- [(j+1)..4]]
  where
    (i, j) = toPair idx

-- | Le plateau de jeu.
data Board = Board { content_ :: IM.IntMap Pawn
                   , elephantsOut_ :: Int
                   , rhinosOut_ :: Int
                   }
  deriving (Eq, Show)

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

-- | Incrémente la réserve pour le type de pion concerné. La fonction n'est
-- pas "sûre" dans le sens où elle peut aboutir à avoir un jeu avec plus que
-- 5 animaux d'un type (répartis entre le plateau et la réserve).
unsafeIncr :: Maybe Pawn -> Board -> Board
unsafeIncr (Just (Animal Elephant _)) b = b { elephantsOut_ = 1 + (elephantsOut_ b) }
unsafeIncr (Just (Animal Rhino _)) b = b { rhinosOut_ = 1 + (rhinosOut_ b) }
unsafeIncr _ b = b

-- | Décrémente la réserve pour le type de pion concerné. La fonction n'est
-- pas "sûre" dans le sens où elle peut aboutir à avoir un jeu avec plus que
-- 5 animaux d'un type (répartis entre le plateau et la réserve).
unsafeDecr :: Pawn -> Board -> Maybe Board
unsafeDecr (Animal Elephant _) b = if elephantsOut_ b > 0
                                   then Just (b { elephantsOut_ = (elephantsOut_ b) - 1})
                                   else Nothing
unsafeDecr (Animal Rhino _) b = if rhinosOut_ b > 0
                                then Just (b { rhinosOut_ = (rhinosOut_ b) - 1})
                                else Nothing
unsafeDecr _ b = Just b

-- | Place le pion (s'il y en a un) de la case indiquée dans la réserve.
pop :: Idx -> Board -> (Board, Maybe Pawn)
pop (Idx i) b = (b', p)
  where
    p = IM.lookup i (content_ b)
    b' = unsafeIncr p (b { content_ = IM.delete i (content_ b) })

-- | Place le pion depuis la réserve (si celle-ci est non vide) sur la case
-- indiquée.
put :: Pawn -> Idx -> Board -> Maybe (Board, Maybe Pawn)
put new (Idx i) b0 = do
  b1 <- unsafeDecr new b0
  let b2 = unsafeIncr old b1
  let b3 = b2 { content_ = IM.insert i new (content_ b0) }
  return (b3, old)
  where
    old = IM.lookup i (content_ b0)
