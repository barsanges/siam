{- |
   Module      : Main
   Copyright   : Copyright (C) 2023 barsanges

Point d'entrée du programme.
-}

module Main where

import Brick ( defaultMain )
import Board ( Faction(..), initialBoard )
import Rules ( Game(..) )
import UI ( app )

-- | Point d'entrée du programme.
main :: IO ()
main = do
    _ <- defaultMain app initialState
    return ()
  where
    initialState = Ongoing Elephant initialBoard
