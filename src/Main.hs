{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      : Main.hs
-- Note        :
--
-- A simple spelling corrector, based off of
-- http://norvig.com/spell-correct.html
-------------------------------------------------------------------------------
module Main where

import           Control.Monad         (forever)
import qualified Data.Trie             as Trie
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

import           Correct.Core

-------------------------------------------------------------------------------
--  Main program
main :: IO ()
main = putStrLn "Loading training data..." >>
       B.readFile "big.txt" >>= \contents ->
       let trained = train . parse $ contents in
       putStrLn (show (Trie.size trained) ++ " words indexed.") >>
       forever (putStrLn "Enter a word: " >>
                B.getLine >>= \word ->
                if length (C.words word) > 1 || lowercase word /= word
                    then putStrLn "Must enter just one lowercase word!"
                    else let corrected = correct trained word in
                        if corrected == word
                            then putStrLn "Your word was spelled correctly"
                            else C.putStrLn $ B.concat ["Did you mean \""
                                                       , corrected
                                                       , "\"?"])
