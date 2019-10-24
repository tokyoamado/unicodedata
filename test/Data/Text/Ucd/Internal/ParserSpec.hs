{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Ucd.Internal.ParserSpec where

import Test.Hspec
import Data.Text.Ucd.Internal.Parser

import qualified Data.Text as T

spec = do
  describe "nameIdx" $ do
    it "single char item" $ do
      nameIdx (Single 1 "a") `shouldBe` 1
    --it "range item" $ do
    --  nameIdx (Range 1 2 "a") `shouldBe` 1
  describe "nameText" $ do
    it "single char item" $ do
      nameText (Single 1 "a") `shouldBe` "a"
    --it "range item" $ do
    --  nameText (Range 1 2 "a") `shouldBe` "a"
  describe "parseBlock" $ do
    it "normal line" $ do
      parseBlock "0000..007F; Basic Latin" `shouldBe` (0, 127, "Basic Latin")
