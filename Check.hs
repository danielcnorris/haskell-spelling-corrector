-- Check.hs
-- A simple spelling corrector, based off of
-- http://norvig.com/spell-correct.html

import           Control.Arrow    ((&&&))
import           Control.Monad    (forever, (>=>))
import           Data.Char        (toLower)
import           Data.List        (foldl', inits, nub, tails)
import qualified Data.Map.Strict  as Map
import           System.IO        (readFile)
import           Text.Regex.Posix ((=~))


----------------------------------------
-- Funciton to load the training data --
----------------------------------------
-- Use bytestrings?
type Words = Map.Map String Int

parse :: String -> [String]
parse = map (map toLower) . concat . (flip (=~) "[a-zA-Z]+")

train :: [String] -> Words
train = foldl' (\m w -> Map.insertWith (+) w 1 m) Map.empty


---------------------------------------------
-- Functions for collecting possible edits --
---------------------------------------------
letters :: [Char]
letters = ['a'..'z']

splits :: String -> [(String, String)]
splits w = map (flip splitAt w) [0..length w]

-- Don't include whole word
splits' :: String -> [(String, String)]
splits' w = map (flip splitAt w) [1..length w - 1]

deletes :: String -> [String]
deletes [] = []
deletes w = splits' w >>= \(first, _:cs) -> return $ first ++ cs

inserts :: String -> [String]
inserts w = splits w >>= \(first, second) ->
            letters >>= \letter ->
            return $ first ++ letter:second

replaces :: String -> [String]
replaces [] = []
replaces w = let strs = if length w == 1
                            then map return letters
                            else splits' w >>= \(first, _:cs) ->
                                 letters >>= \letter ->
                                 return $ first ++ letter:cs
             in filter ((/=) w) strs

transposes :: String -> [String]
transposes w = if length w <= 1
                  then []
                  else splits' w >>= \(first, c:cs) ->
                       return $ init first ++ [c, last first] ++ cs

known :: Words -> [String] -> [String]
known ws = filter (flip Map.member ws)

-- Any way to do point free?
edits :: Int -> String -> [String]
edits n = foldl' (>=>) return (replicate n edits')
    where edits' w = nub $ [deletes, inserts, replaces, transposes] >>=
                     ($ w)


---------------------------------------
-- Main spelling suggestion function --
---------------------------------------
-- Take lower case of word?
-- Enforce just one word at a time?
correct :: Words -> String -> String
correct ws w =  snd .
                maximum .
                map ((flip (Map.findWithDefault 1) ws)  &&& id).
                head .
                filter (not . null) .
                map ($ w) $
                [ known ws . return
                , known ws . edits 1
                , known ws . edits 2
                , return]

---------------------
-- Main executable --
---------------------
main :: IO ()
main = do
    contents <- readFile "big.txt"
    let parsed = parse contents
    print $ length parsed
    print "That's a big file"
    let words = train parsed
    print $ Map.size words
    print "That's a big dictionary"
{--
main = readFile "big.txt" >>= \contents ->
       forever $ getLine >>=
                 putStrLn . correct (train . parse $ contents)
--}
