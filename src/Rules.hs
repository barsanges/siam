{- |
   Module      : Rules
   Copyright   : Copyright (C) 2023 barsanges

Implémentation des règles de Siam.
-}

module Rules
  ( Game(..)
  , Move(..)
  , parseMove
  ) where

import Board
import Utils

-- | La partie, soit en cours soit terminée.
data Game = Ongoing Faction Board
          | Ended Faction Board
  deriving Show

-- | Un coup, non-nécessairement admissible.
data Move = Enter Idx Orientation
          | Movement Idx Idx Orientation
          | Exit Idx
  deriving (Eq, Show)

-- | Analyse une commande décrivant un coup.
parseMove :: String -> Maybe Move
parseMove cmd = case words (sanitizeStr cmd) of
  "r":y:z:[] -> do
    y' <- parseIdx y
    z' <- parseOrientation z
    return (Enter y' z')
  x:y:z:[] -> do
    x' <- parseIdx x
    y' <- parseIdx y
    z' <- parseOrientation z
    return (Movement x' y' z')
  x:"r":[] -> do
    x' <- parseIdx x
    return (Exit x')
  x:y:[] -> do
    x' <- parseIdx x
    y' <- parseOrientation y
    return (Movement x' x' y')
  _ -> Nothing
