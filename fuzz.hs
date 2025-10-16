import System.Random (randomRIO)
import Data.List (isPrefixOf, tails, nub)
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
  len <- randomRIO (10 :: Int, 15)
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

oneStepWords :: String -> [(String, String)] -> [String]
oneStepWords word rules = nub $ concatMap (applyRule word) rules
  where
    applyRule w (pat, rep) = [replaceAt w pat rep i | (i, suf) <- zip [0..] (tails w), pat `isPrefixOf` suf]

reachableWords :: String -> [(String, String)] -> Int -> [String]
reachableWords start rules maxDepth = go maxDepth [start] [start]
  where
    go 0 _ visited = visited
    go n border visited =
      let next = nub [w' | w <- border, w' <- oneStepWords w rules, w' `notElem` visited]
      in if null next
         then visited
         else go (n - 1) next (visited ++ next)

checkUnion :: String -> String -> [(String, String)] -> Int -> Bool
checkUnion w0 w1 rules maxDepth =
  any (`elem` reachable0) reachable1
  where
    reachable0 = reachableWords w0 rules maxDepth
    reachable1 = reachableWords w1 rules maxDepth

main :: IO ()
main = do
  initial <- generateRandomWord
  putStrLn $ "Initial word: " ++ initial

  rewritten <- rewriteRandomly initial t1
  putStrLn $ "Rewritten word: " ++ rewritten

  let res = checkUnion initial rewritten t2 100
  print res
