{- |
   Module      : UtilsSpec
   Copyright   : Copyright (C) 2023 barsanges

Teste le module Utils.
-}

module UtilsSpec ( spec ) where

import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "toLowerStr" $ do
    it "converts all letters to the corresponding lower-case letter (1)" $
      toLowerStr "FOO" `shouldBe` "foo"

    it "converts all letters to the corresponding lower-case letter (2)" $
      toLowerStr " Bar 123 " `shouldBe` " bar 123 "

    it "converts all letters to the corresponding lower-case letter (3)" $
      toLowerStr " QuX " `shouldBe` " qux "

  describe "trimStr" $ do
    it "removes leading and trailing whitespaces (1)" $
      trimStr "FOO" `shouldBe` "FOO"

    it "removes leading and trailing whitespaces (2)" $
      trimStr " Bar 123 " `shouldBe` "Bar 123"

    it "removes leading and trailing whitespaces (3)" $
      trimStr " QuX " `shouldBe` "QuX"

  describe "sanitizeStr" $ do
    it "combines 'toLowerStr' and 'trimStr' (1)" $
      sanitizeStr "FOO" `shouldBe` "foo"

    it "combines 'toLowerStr' and 'trimStr' (2)" $
      sanitizeStr " Bar 123 " `shouldBe` "bar 123"

    it "combines 'toLowerStr' and 'trimStr' (3)" $
      sanitizeStr " QuX " `shouldBe` "qux"
