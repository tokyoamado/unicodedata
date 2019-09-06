{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Text.Ucd.Char
  ( nameOf
  , charOf
  , blockOf
  , charsOf
  ) where

import Data.Char (chr, ord, toLower, toUpper)
import Language.Haskell.TH (runIO)
import Numeric (showHex)
import qualified Data.IntMap as I
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Text.Ucd.Internal.Parser

cpStr :: Char -> String
cpStr c = drop d "000" ++ raw
  where
    raw = map toUpper $ showHex (ord c) ""
    len = length raw
    d = if len <= 4 then len - 1 else len - 3

nameOf :: Char -> String
nameOf c = case I.lookupLE (ord c) nameDic of
  Just (_, Single cp n) -> if ord c == cp then n else "U+" ++ cpStr c
  Just (_, Range b e n) -> if ord c <= e then init n ++ cpStr c else "U+" ++ cpStr c
  Nothing               -> "U+" ++ cpStr c

charOf :: String -> Maybe Char
charOf s = case M.lookup (norm s) nameDicRev of
  Just (Single cp _) -> Just $ chr cp
  Just (Range b e n) -> Nothing  -- Don't support range.
  Nothing -> Nothing
  where
    norm = map toUpper . filter (\c -> c `notElem` (" -_" :: String))

blockOf :: Char -> String
blockOf c = case I.lookupLE (ord c) blockDic of
  Just (_, (e, n)) -> if ord c <= e then n else "No_Block"
  Nothing           -> "No_Block"

charsOf :: String -> [Char]
charsOf block = case M.lookup n blockDicRev of
  Just (b, e) -> [b .. e]
  Nothing     -> []
  where
    n = (map toLower . filter (\c -> c `notElem` (" -_" :: String))) block

(blockDic, blockDicRev) = $(do
  let norm = T.toLower . T.filter (\c -> T.all (/= c) " -_")
  blocks_txt <- runIO $ map parseBlock . filter (\s -> s /= "" && T.head s /= '#') . map T.strip . T.lines <$> T.readFile "ucd/Blocks.txt"
  let (dic, dicRev) = unzip $ map (\(b, e, n) -> ((b, (e, T.unpack n)), ((T.unpack . norm) n, (chr b, chr e)))) blocks_txt
  [| (I.fromList dic, M.fromList dicRev) |]
  )

(nameDic, nameDicRev) = $(do
  let norm = map toUpper . filter (\c -> c `notElem` (" -_" :: String))
  name_txt <- runIO $ map parseName . filter (\s -> s /= "" && T.head s /= '#') . map T.strip . T.lines <$> T.readFile "ucd/extracted/DerivedName.txt"
  let (dic, dicRev) = unzip $ map (\name -> ((nameIdx name, name), ((norm . nameText) name, name))) name_txt
  [| (I.fromList dic, M.fromList dicRev) |]
  )
