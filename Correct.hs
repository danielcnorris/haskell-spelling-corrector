{-# LANGUAGE OverloadedStrings #-}
-- Correct.hs
-- A simple spelling corrector, based off of
-- http://norvig.com/spell-correct.html

import           Control.Arrow            ((&&&))
import           Control.Monad            (forever, (>=>))
import qualified Data.Array.Unboxed       as AU
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as BI
import           Data.Char                (toLower)
import           Data.List                (foldl', nub)
import qualified Data.Map.Strict          as Map
import           Data.Word                (Word8)
import qualified Data.Word                as W
import           Text.Regex.Posix         ((=~))

-------------------------------------
-- Helper functions for ByteString --
-------------------------------------
toLowerW8 :: AU.UArray W.Word8 W.Word8
toLowerW8 = AU.listArray (0,255)  (map (BI.c2w . toLower) ['\0'..'\255'])

lowercase :: ByteString -> ByteString
lowercase = B.map (\x -> toLowerW8 AU.! x)


----------------------------------------
-- Funciton to load the training data --
----------------------------------------
type Words = Map.Map ByteString Int

parse :: ByteString -> [ByteString]
parse = map lowercase . concat . flip (=~) ("[a-zA-Z]+" :: ByteString)

train :: [ByteString] -> Words
train = foldl' (\m w -> Map.insertWith (+) w 1 m) Map.empty


---------------------------------------------
-- Functions for collecting possible edits --
---------------------------------------------
letters :: [Word8]
letters = map (BI.c2w . toLower) ['\0'..'\255']

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

known :: Words -> [ByteString] -> [ByteString]
known ws = filter (`Map.member` ws)

edits :: Int -> ByteString -> [ByteString]
edits n = foldl' (>=>) return (replicate n edits')
    where edits' w = nub $ [deletes, inserts, replaces, transposes] >>= ($ w)


---------------------------------------
-- Main spelling suggestion function --
---------------------------------------
correct :: Words -> ByteString -> ByteString
correct ws w =  snd .
                maximum .
                map (flip (Map.findWithDefault 1) ws  &&& id).
                head .
                filter (not . null) .
                map ($ w) $
                [ known ws . return
                , known ws . edits 1
                , known ws . edits 2
                , return
                ]

------------------
-- Main program --
------------------
main :: IO ()
main = do
    putStrLn "Loading training data..."
    contents <- B.readFile "big.txt"
    let words = train . parse $ contents
    putStrLn $ show (Map.size words) ++ " words indexed."
    forever $ do
        putStrLn "Enter a word: "
        word <- B.getLine
        if B.length word > 1 || lowercase word /= word
            then putStrLn "Must enter just one lower case word!"
            else let corrected = correct words word in
                 if corrected == word
                     then putStrLn "Your word was spelled correctly"
                     else C.putStrLn $ B.concat ["Did you mean: ", corrected]
