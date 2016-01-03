module AdventOfCode.Day5(
  niceStrings,
  niceStrings2)  where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

vowels :: String -> String
vowels input = filter (\e -> elem e "aeiou") input

-- It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
containsAtLeast3Vowels :: String -> Bool
containsAtLeast3Vowels input = length (vowels input) >= 3


-- It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
containsAtLeastOneLetterAppearingTwiceInARow :: String -> Bool
containsAtLeastOneLetterAppearingTwiceInARow input = windowSrch '1' input
    where windowSrch existing (x : xs)
            | x == existing && x >= 'a' && x <= 'z' = True
            | otherwise                         = windowSrch x xs
          windowSrch _        [] = False


-- It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
noBadString :: String -> Bool
noBadString input = windowSrch '1' input
    where windowSrch existing (x : xs)
            | elem [existing, x] badStrs = False
            | otherwise                                 = windowSrch x xs
          windowSrch _        [] = True
          badStrs = ["ab", "cd", "pq", "xy"]


niceString :: String -> Bool
niceString str = (containsAtLeast3Vowels str) && (containsAtLeastOneLetterAppearingTwiceInARow str) && (noBadString str)


niceStrings :: [String] -> [String]
niceStrings inputs = filter niceString inputs



-- It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa),
--but not like aaa (aa, but it overlaps).
windowedInput :: Int -> [a] -> [[a]]
windowedInput windowSize input = fpa
  where
    fpa = if length input <= windowSize then [input] else pa [] Nothing input
    pa res (Just existing) (x : xs) = pa ([existing , x] : res) (Just x) xs
    pa res Nothing         (x : xs) = pa res (Just x) xs
    pa res _               []       = reverse res

usedIndices :: [[(Int, Char)]] -> Set Int
usedIndices (h : t)  = Set.union (Set.fromList (fmap fst h)) (usedIndices t)
usedIndices []       = Set.empty

notReused :: Set Int -> [(Int, Char)] -> Bool
notReused used nxt = Set.size(Set.intersection (usedIndices [nxt]) used) == 0

removeDupes :: [[(Int, Char)]] -> [[(Int, Char)]]
removeDupes subLst = foldr fc [] subLst
  where
        fc nextT keeps = if (notReused used nextT) then (nextT : keeps) else keeps
          where
            used = usedIndices keeps

shouldKeep :: [[(Int, Char)]] -> Bool
shouldKeep subLst = length (removeDupes subLst) > 1

p2Str :: [(Int, Char)] -> String
p2Str = fmap snd

eqAsStr :: [(Int, Char)] -> [(Int, Char)] -> Bool
eqAsStr s1 s2 = (p2Str s1) == (p2Str s2)

cmpAsStr :: [(Int, Char)] -> [(Int, Char)] -> Ordering
cmpAsStr s1 s2 = (p2Str s1) `compare` (p2Str s2)

nonOverLappingDuplicatePairs :: String -> Bool
nonOverLappingDuplicatePairs input = length (filter shouldKeep grps) > 0
    where
      zippedWith = zip [0..] input
      windowed = windowedInput 2 zippedWith
      grps = List.groupBy eqAsStr (List.sortBy cmpAsStr windowed) :: [[[(Int, Char)]]]

-- It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
hashPairWithMiddleV :: String -> Bool
hashPairWithMiddleV input = hasPair '1' '1' input
    where
      hasPair minus2 minus1 [] = False
      hasPair minus2 minus1 (x:xs)
        | minus2 == x       = True
        | otherwise         = hasPair minus1 x xs

niceString2 :: String -> Bool
niceString2 str = (nonOverLappingDuplicatePairs str) && (hashPairWithMiddleV str)

niceStrings2 :: [String] -> [String]
niceStrings2 inputs = filter niceString2 inputs

