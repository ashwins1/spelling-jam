module Main where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Control.Applicative
import Data.Char
import Control.Arrow
import Data.Ord
import Data.List

type Hist = M.HashMap String Int

mkHistogram :: String -> IO Hist
mkHistogram f = M.fromListWith (+) . flip zip (repeat 1) . map (T.unpack . T.filter isAlpha) . T.words . T.toLower <$> T.readFile f

oneStepEdits word = concat $ [deletes, transposes, replaces, inserts]
    where len = length word

          wordSplits :: [(String,String)]
          wordSplits = map (flip splitAt word) [0.. fromIntegral len]

          deletes :: [String]
          deletes = map (uncurry mkDelete) wordSplits
          mkDelete xs (y:ys) = xs ++ ys
          mkDelete xs ys = xs ++ ys

          transposes :: [String]
          transposes = map (uncurry mkTranspose) wordSplits
          mkTranspose xs (y:y':ys) = xs ++ (y':y:ys)
          mkTranspose xs ys = xs ++ ys

          replaces :: [String]
          replaces = concatMap (uncurry mkReplace) wordSplits
          mkReplace xs (_:ys) = map (\c -> xs ++ (c:ys)) ['a'..'z']
          mkReplace xs ys = [xs ++ ys]

          inserts :: [String]
          inserts = concatMap (uncurry mkInsert) wordSplits
          mkInsert xs ys = map (\c -> xs ++ (c:ys)) ['a'..'z']

-- twoStepEdits word = concatMap oneStepEdits (oneStepEdits word)
--
topEdit :: Hist -> [String] -> (String,Int)
topEdit hist ws = maximumBy (comparing snd) $ map (\w -> (w,M.lookupDefault 0 w hist)) ws

correct hist word
  | word `M.member` hist = word
  | snd topOne > 0 = fst topOne
  | snd topTwo > 0 = fst topTwo
  | otherwise = word
    where
      oneSteps = oneStepEdits word
      twoSteps = concatMap oneStepEdits oneSteps
      topOne = topEdit hist oneSteps
      topTwo = topEdit hist twoSteps

data Edit = Deletion
          | Insertion
          | Substitution
          | Transposition

findEditSequence :: String -> String -> [Edit]
findEditSequence word misspelling
