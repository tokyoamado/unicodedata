{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Ucd.Internal.Parser where

import Data.Attoparsec.Text (Parser, char, hexadecimal, parseOnly, skipSpace, string, takeText)
import qualified Data.Text as T

parseLine :: T.Text -> (Int, Int, T.Text)
parseLine t = case parseOnly line t of
  Right (begin, end, text) -> (begin, end, text)
  Left err                 -> error err

line :: Parser (Int, Int, T.Text)
line = do
  begin <- hexadecimal
  string ".."
  end <- hexadecimal
  char ';'
  skipSpace
  name <- takeText
  skipSpace
  return (begin, end, name)
