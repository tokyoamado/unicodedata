{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Text.Ucd.Char
  ( nameOf
  , charOf
  , blockOf
  , charsOf
  ) where

import Data.Char (chr, ord, toUpper)
import Language.Haskell.TH (runIO)
import qualified Data.IntMap as I
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Text.Ucd.Internal.Parser

nameOf :: Char -> String
nameOf c = "undefined"

charOf :: String -> Char
charOf s = 'u'

blockOf :: Char -> String
blockOf c = case I.lookupLE (ord c) blockDic of
  Just (_, (e, n)) -> if ord c <= e then n else "No_Block"
  Nothing           -> "No_Block"

charsOf :: String -> [Char]
charsOf block = case M.lookup n blockDicRev of
  Just (b, e) -> [b .. e]
  Nothing     -> []
  where
    n = (map toUpper . filter (\c -> c `notElem` (" -_" :: String))) block

(blockDic, blockDicRev) = $(do
  let norm = T.toUpper . T.filter (\c -> T.all (/= c) " -_")
  blocks_txt <- runIO $ map parseLine . filter (\s -> s /= "" && T.head s /= '#') . map T.strip . T.lines <$> T.readFile "ucd/Blocks.txt"
  let (dic, dicRev) = unzip $ map (\(b, e, n) -> ((b, (e, T.unpack n)), ((T.unpack . norm) n, (chr b, chr e)))) blocks_txt
  [| (I.fromList dic, M.fromList dicRev) |]
  )
