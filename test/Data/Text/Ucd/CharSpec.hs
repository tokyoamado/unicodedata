module Data.Text.Ucd.CharSpec (spec) where

import Test.Hspec
import Data.Text.Ucd.Char

spec = do
  describe "nameOf" $ do
    it "'A' should be 'LATIN CAPITAL LETTER A'" $ do
      nameOf 'A' `shouldBe` "LATIN CAPITAL LETTER A"

  describe "blockOf" $ do
    it "'A' should be 'Basic Latin'" $ do
      blockOf 'A' `shouldBe` "Basic Latin"

  describe "charsOf" $ do
    it "'Basic Latin' should be ['\x00'..'\x7f']" $ do
      charsOf "Basic Latin" `shouldBe` ['\x00'..'\x7f']
