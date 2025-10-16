import System.Random (randomRIO)
import Data.List (isPrefixOf, tails)
import Control.Monad (replicateM)

t1 :: [(String, String)]
t1 =
  [ ("babb", "bbbab")
  , ("baabb", "babbaab")
  , ("baaabb", "baabbaaab")
  , ("bbbb", "abab")
  , ("aaaa", "a")
  ]

t2 :: [(String, String)]
t2 =
  [ ("aaaa", "a")
  , ("bbbb", "abab")
  , ("babab", "ababb")
  , ("babbb", "ababb")
  , ("bbabb", "ababb")
  , ("bbbab", "babb")
  , ("aababb", "ababb")
  , ("abaabb", "ababb")
  , ("baabab", "ababb")
  , ("baabbb", "ababb")
  , ("babbab", "ababb")
  , ("bbaabb", "ababb")
  , ("abaaabb", "ababb")
  , ("baaabab", "ababb")
  , ("baaabbb", "ababb")
  , ("baabbab", "ababb")
  , ("babbaab", "baabb")
  , ("bbaaabb", "ababb")
  , ("baaabbab", "ababb")
  , ("ababbaaab", "ababb")
  , ("baabbaaab", "baaabb")
  ]

alphabet :: [Char]
alphabet = ['a', 'b']

generateRandomWord :: IO String
generateRandomWord = do
  len <- randomRIO (10 :: Int, 20)
  replicateM len (randomChoice alphabet)

randomChoice :: [a] -> IO a
randomChoice xs = do
  i <- randomRIO (0, length xs - 1 :: Int)
  return (xs !! i)

rewriteRandomly :: String -> [(String, String)] -> IO String
rewriteRandomly word rules = do
  n <- randomRIO (5 :: Int, 10)
  loop n word
  where
    loop 0 w = return w
    loop k w = do
      let matches = findMatches w rules
      if null matches
        then return w
        else do
          (pat, rep, idx) <- randomChoice matches
          let newWord = replaceAt w pat rep idx
          loop (k - 1) newWord

findMatches :: String -> [(String, String)] -> [(String, String, Int)]
findMatches word rules = concatMap findInWord rules
  where
    findInWord (pat, rep) = [(pat, rep, i) | (i, suf) <- zip [0..] (tails word) , pat `isPrefixOf` suf]

replaceAt :: String -> String -> String -> Int -> String
replaceAt word pat rep idx =
  take idx word ++ rep ++ drop (idx + length pat) word

countOverlapping :: String -> String -> Int
countOverlapping s sub = length [() | suf <- tails s, sub `isPrefixOf` suf]

countTrailingA :: String -> Int
countTrailingA = length . takeWhile (=='a') . reverse

inv1 :: String -> String -> Bool
inv1 a b = last a == last b

inv2 :: String -> String -> Bool
inv2 a b = countTrailingA a `mod` 3 == countTrailingA b `mod` 3

inv3 :: String -> String -> Bool
inv3 a b =
  let resA = (sum (map (countOverlapping a) ["ba", "bb", "b"])) `mod` 2
      resB = (sum (map (countOverlapping b) ["ba", "bb", "b"])) `mod` 2
  in resA <= resB

inv4 :: String -> String -> Bool
inv4 a b =
  let resA = countOverlapping a "ab" + countOverlapping a "bb" - countOverlapping a "b"
      resB = countOverlapping b "ab" + countOverlapping b "bb" - countOverlapping b "b"
  in resA <= resB

main :: IO ()
main = do
  result <- test 10 True
  putStrLn $ replicate 30 '-' 
  print result

test :: Int -> Bool -> IO Bool
test 0 ok = return ok
test n ok = do
  initial <- generateRandomWord
  putStrLn $ "Initial word: " ++ initial
  rewritten1 <- rewriteRandomly initial t1
  rewritten2 <- rewriteRandomly initial t2
  putStrLn $ "Rewritten by t1: " ++ rewritten1
  putStrLn $ "Rewritten by t2: " ++ rewritten2
  let res =
        inv1 initial rewritten1 &&
        inv2 initial rewritten1 &&
        inv3 initial rewritten1 &&
        inv4 initial rewritten1 &&
        inv1 initial rewritten2 &&
        inv2 initial rewritten2 &&
        inv3 initial rewritten2 &&
        inv4 initial rewritten2
  if not res
    then do
      return False
    else do
      print res
      test (n - 1) ok
