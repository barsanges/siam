{- |
   Module      : RulesSpec
   Copyright   : Copyright (C) 2023 barsanges

Teste le module Rules.
-}

module RulesSpec ( spec ) where

import Data.Maybe ( fromJust )
import Test.Hspec
import Board
import Rules

unsafeIdx :: String -> Idx
unsafeIdx = fromJust . parseIdx

spec :: Spec
spec = do
  describe "parseMove" $ do
    it "a movement is denoted by 'index index orientation'" $
      parseMove "a1 a2 w" `shouldBe` Just (Movement (unsafeIdx "a1") (unsafeIdx "a2") West)

    it "a rotation is denoted by 'index orientation'" $
      parseMove "b5 e" `shouldBe` Just (Movement (unsafeIdx "b5") (unsafeIdx "b5") East)

    it "an entry is denoted by 'r index orientation'" $
      parseMove "r e3 n" `shouldBe` Just (Enter (unsafeIdx "e3") North)

    it "an index is denoted by 'index r'" $
      parseMove "c5 r" `shouldBe` Just (Exit (unsafeIdx "c5"))

    it "illegal moves are also parsed" $
      parseMove "c4 r" `shouldBe` Just (Exit (unsafeIdx "c4"))

    it "case is not relevant" $
      parseMove "A1 A2 W" `shouldBe` Just (Movement (unsafeIdx "a1") (unsafeIdx "a2") West)

    it "white spaces at the beginning and the end of the string are irrelevant" $
      parseMove "  c4 r " `shouldBe` Just (Exit (unsafeIdx "c4"))

    it "erroneous strings are not parsed (1)" $
      parseMove "foo bar" `shouldBe` Nothing

    it "erroneous strings are not parsed (2)" $
      parseMove "w a1 a2" `shouldBe` Nothing

    it "erroneous strings are not parsed (3)" $
      parseMove "r c4" `shouldBe` Nothing
