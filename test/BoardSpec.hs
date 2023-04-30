{- |
   Module      : BoardSpec
   Copyright   : Copyright (C) 2023 barsanges

Teste le module Board.
-}

module BoardSpec ( spec ) where

import Data.Maybe ( fromJust )
import qualified Data.Set as S
import Test.Hspec
import Board

unsafeIdx :: String -> Idx
unsafeIdx = fromJust . parseIdx

spec :: Spec
spec = do
  describe "parseOrientation" $ do
    it "'N' stands for 'North'" $
      parseOrientation "N" `shouldBe` Just North

    it "'W' stands for 'West'" $
      parseOrientation "W" `shouldBe` Just West

    it "'S' stands for 'South'" $
      parseOrientation "S" `shouldBe` Just South

    it "'E' stands for 'East'" $
      parseOrientation "E" `shouldBe` Just East

    it "case is not relevant" $
      parseOrientation "n" `shouldBe` Just North

    it "white spaces at the beginning and the end of the string are irrelevant" $
      parseOrientation "  s " `shouldBe` Just South

    it "erroneous strings are not parsed (1)" $
      parseOrientation "foo" `shouldBe` Nothing

    it "erroneous strings are not parsed (2)" $
      parseOrientation "west" `shouldBe` Nothing

    it "erroneous strings are not parsed (3)" $
      parseOrientation "m n o" `shouldBe` Nothing

  describe "parseIdx" $ do
    it "an index is a combination of a letter and a digit (1)" $
      -- fromJust des 2 côtés car on veut s'assurer que le test plante
      -- si les deux fonctions renvoient Nothing.
      fromJust (parseIdx "a4") `shouldBe` fromJust (mkIdx 5)

    it "an index is a combination of a letter and a digit (2)" $
      fromJust (parseIdx "d5") `shouldBe` fromJust (mkIdx 3)

    it "an index is a combination of a letter and a digit (3)" $
      fromJust (parseIdx "e1") `shouldBe` fromJust (mkIdx 24)

    it "case is not relevant" $
      fromJust (parseIdx "B2") `shouldBe` fromJust (mkIdx 16)

    it "white spaces at the beginning and the end of the string are irrelevant" $
      fromJust (parseIdx  " c3  ") `shouldBe` fromJust (mkIdx 12)

    it "erroneous strings are not parsed (1)" $
      parseIdx "foo" `shouldBe` Nothing

    it "erroneous strings are not parsed (2)" $
      parseIdx "a5foo" `shouldBe` Nothing

    it "erroneous strings are not parsed (3)" $
      parseIdx "a5 e1" `shouldBe` Nothing

  describe "Idx" $ do
    it "the output of show is equal to the input of parseIdx (1)" $
      show (parseIdx "a1") `shouldBe` "Just a1"

    it "the output of show is equal to the input of parseIdx (2)" $
      show (parseIdx "c2") `shouldBe` "Just c2"

    it "the output of show is equal to the input of parseIdx (3)" $
      show (parseIdx "e4") `shouldBe` "Just e4"

    it "the output of show is equal to the input of parseIdx (4)" $
      show (parseIdx "b5") `shouldBe` "Just b5"

    it "the output of show is equal to the input of parseIdx (5)" $
      show (parseIdx "d3") `shouldBe` "Just d3"

  describe "edge" $ do
    it "returns the indexes of the cells on the edge of the board" $
      edge `shouldBe` (S.fromList
                       $ [unsafeIdx [c, r]
                         | c <- ['a', 'b', 'c', 'd', 'e']
                         , r <- ['1', '5']]
                       ++ [unsafeIdx [c, r]
                          | c <- ['a', 'e']
                          , r <- ['5', '4', '3', '2', '1']])

  describe "neighbors" $ do
    it "returns the indexes of the neighboring cells (1)" $
      neighbors (fromJust . parseIdx $ "a5") `shouldBe` (S.fromList [unsafeIdx x | x <- ["b5", "a4"]])

    it "returns the indexes of the neighboring cells (2)" $
      neighbors (fromJust . parseIdx $ "b1") `shouldBe` (S.fromList [unsafeIdx x | x <- ["a1", "b2", "c1"]])

    it "returns the indexes of the neighboring cells (3)" $
      neighbors (fromJust . parseIdx $ "d2") `shouldBe` (S.fromList [unsafeIdx x | x <- ["c2", "d3", "e2", "d1"]])

    it "returns the indexes of the neighboring cells (4)" $
      neighbors (fromJust . parseIdx $ "c3") `shouldBe` (S.fromList [unsafeIdx x | x <- ["b3", "c4", "d3", "c2"]])

    it "returns the indexes of the neighboring cells (5)" $
      neighbors (fromJust . parseIdx $ "e4") `shouldBe` (S.fromList [unsafeIdx x | x <- ["d4", "e5", "e3"]])

  describe "line" $ do
    it "returns the indexes of the cells in a given direction from a given starting point (1)" $
      line (unsafeIdx "b5") South `shouldBe` [unsafeIdx x | x <- ["b4", "b3", "b2", "b1"]]

    it "returns the indexes of the cells in a given direction from a given starting point (2)" $
      line (unsafeIdx "c3") West `shouldBe` [unsafeIdx x | x <- ["b3", "a3"]]

    it "returns the indexes of the cells in a given direction from a given starting point (3)" $
      line (unsafeIdx "a4") North `shouldBe` [unsafeIdx "a5"]

    it "returns the indexes of the cells in a given direction from a given starting point (4)" $
      line (unsafeIdx "d2") East `shouldBe` [unsafeIdx "e2"]

    it "returns the indexes of the cells in a given direction from a given starting point (5)" $
      line (unsafeIdx "c1") South `shouldBe` []
