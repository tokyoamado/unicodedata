{-# LANGUAGE OverloadedStrings, DeriveLift #-}

module Data.Text.Ucd.Internal.Parser
  ( Name (Single, Range)
  , nameIdx
  , nameText
  , parseBlock
  , parseName
  ) where

import Data.Attoparsec.Text (Parser, char, hexadecimal, option, parseOnly, skipSpace, string, takeText)
import Language.Haskell.TH.Syntax (Lift)
import qualified Data.Text as T

parseBlock :: T.Text -> (Int, Int, T.Text)
parseBlock t = case parseOnly blockline t of
  Right (begin, end, text) -> (begin, end, text)
  Left err                 -> error err

blockline :: Parser (Int, Int, T.Text)
blockline = do
  begin <- hexadecimal
  string ".."
  end <- hexadecimal
  char ';'
  skipSpace
  name <- takeText
  return (begin, end, name)

data Name = Single Int String | Range Int Int String deriving (Lift, Show)
nameIdx :: Name -> Int
nameIdx (Single i _)  = i
nameIdx (Range i _ _) = i
nameText :: Name -> String
nameText (Single _ n)  = n
nameText (Range _ _ n) = n

parseName :: T.Text -> Name
parseName t = case parseOnly nameline t of
  Right name -> name
  Left err   -> error err

nameline :: Parser Name
nameline = do
  begin <- hexadecimal
  end <- option (-1) $ string ".." *> hexadecimal
  skipSpace
  char ';'
  skipSpace
  name <- takeText
  return $ if end == (-1) then Single begin (T.unpack name) else Range begin end (T.unpack name)
