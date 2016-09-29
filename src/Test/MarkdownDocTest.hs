{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.MarkdownDocTest (mddoctest) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Managed     (with)
import           Data.List                 (intercalate)
import           Data.Monoid
import           Data.Text                 (pack, unpack)
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (unlines)
import           Test.DocTest
import           Text.Pandoc
import           Text.Pandoc.Walk          (walk)
import           Turtle.Prelude

unlines :: [String] -> String
unlines = intercalate "\n"

doctestify :: Block -> Block
doctestify c@(CodeBlock (_,xs,_) _)
  | "haskell" `notElem` xs || "literate" `notElem` xs = c
doctestify (CodeBlock (i,xs,ks) cb)
  | "example" `elem` xs =
    CodeBlock (i,xs,ks) ("\n" ++ comment ("| >>> " ++ cb) ++ "\n")
  | "prop" `elem` xs =
    CodeBlock (i,xs,ks) ("\n-- | prop> " ++ cb ++ "\n")
  | otherwise =
    CodeBlock (i,xs,ks) ("\n" ++ cb ++ "\n")
doctestify b = b

comment :: String -> String
comment s = unlines [ "-- " <> l | l <- lines s ]

mddoctest :: [String] -> IO ()
mddoctest filenames = do
  let files = map decodeString filenames
  hmdir <- home
  forM_ files $ \file -> do
    cont <- readTextFile file
    pand <- either (fail . show) pure (readMarkdown def (unpack cont))
    let doctested = walk doctestify pand
    let out = pack (writeMarkdown (def { writerExtensions = def <> [Ext_literate_haskell]}) doctested)
    let tmpname = (either id id . toText . filename) file
    with (mktempdir hmdir tmpname) $ \tmpDir -> do
      let tmpFile = tmpDir <> "Main.lhs"
      writeTextFile tmpFile out
      doctest [encodeString tmpFile]
      rm tmpFile
