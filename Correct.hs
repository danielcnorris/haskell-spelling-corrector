{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Correct.hs
-- Note        :
--
-- A simple spelling corrector, based off of
-- http://norvig.com/spell-correct.html
-------------------------------------------------------------------------------

import           Control.Arrow            ((&&&))
import           Control.Monad            (forever)
import qualified Data.Array.Unboxed       as AU
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as BI
import           Data.Char                (toLower)
import           Data.List                (foldl')
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.Trie                as Trie
import           Data.Trie.Convenience    (insertWith, lookupWithDefault)
import           Data.Word                (Word8)
import qualified Data.Word                as W
import           Text.Regex.Posix         ((=~))

-------------------------------------------------------------------------------
--  Helper functions for ByteString
toLowerW8 :: AU.UArray W.Word8 W.Word8
toLowerW8 = AU.listArray (0,255)  (map (BI.c2w . toLower) ['\0'..'\255'])

lowercase :: ByteString -> ByteString
lowercase = B.map (\x -> toLowerW8 AU.! x)


-------------------------------------------------------------------------------
--  Functions to load the training data
type Words = Trie.Trie Int

parse :: ByteString -> [ByteString]
parse = map lowercase . concat . flip (=~) ("[a-zA-Z]+" :: ByteString)

train :: [ByteString] -> Words
train = foldl' (\t w -> insertWith (+) w 1 t) Trie.empty


-------------------------------------------------------------------------------
--  Functions for collecting possible edits
letters :: [Word8]
letters = map (BI.c2w . toLower) (['\65'..'\90'] ++ ['\97'..'\122'])

splits ::  ByteString -> [(ByteString, ByteString)]
splits w = map (`B.splitAt` w) [0..B.length w]

-- Don't include whole word
splits' :: ByteString -> [(ByteString, ByteString)]
splits' w = map (`B.splitAt` w) [1..B.length w - 1]

deletes :: ByteString -> [ByteString]
deletes w = if B.null w
                then []
                else splits' w >>= \(first, second) ->
                     return $ B.concat [first, B.tail second]

inserts :: ByteString -> [ByteString]
inserts w = splits w >>= \(first, second) ->
            letters >>= \letter ->
            return $ B.concat [first, B.cons letter second]

replaces :: ByteString -> [ByteString]
replaces w = if B.null w
                 then []
                 else let strs = if B.length w == 1
                          then return $ B.pack letters
                          else splits' w >>= \(first, second) ->
                               letters >>= \letter ->
                               let second' = B.cons letter (B.tail second) in
                               return $ B.concat [first, second']
                      in filter (w /=) strs

transposes :: ByteString -> [ByteString]
transposes w = if B.length w <= 1
                  then []
                  else splits' w >>= \(first, second) ->
                       return $ B.concat [ B.init first
                                         ,  B.pack [ B.head second
                                                   , B.last first
                                                   ]
                                         , B.tail second
                                         ]

known :: Words -> Set.Set ByteString -> Set.Set ByteString
known ws = Set.filter (`Trie.member` ws)

edits1 :: ByteString -> Set.Set ByteString
edits1 w = foldl' (\acc f -> Set.union (foldl' (flip Set.insert) Set.empty
                   (f w)) acc)
               Set.empty [deletes, inserts, replaces, transposes]

knownEdits2 :: Words -> ByteString -> Set.Set ByteString
knownEdits2 ws w = Set.fold (\x acc -> Set.union acc (known ws . edits1 $ x))
                       Set.empty (edits1 w)

-------------------------------------------------------------------------------
--  Main spelling suggestion function
correct :: Words -> ByteString -> ByteString
correct ws w =  snd .
                Set.findMax .
                Set.map (flip (lookupWithDefault 1) ws  &&& id) .
                head .
                filter (not . Set.null) .
                map ($ w) $
                [ known ws . Set.singleton
                , known ws . edits1
                , knownEdits2 ws
                , Set.singleton
                ]


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
