module AdventOfCode.Day3 (
    housesVisited,
    housesVisitedWithRoboSanta
    ) where
import Data.Set (Set)
import qualified Data.Set as Set

nextPosition :: Char -> (Int, Int) -> (Int, Int)
nextPosition '^' (x, y) = (x, y+1)
nextPosition 'v' (x, y) = (x, y-1)
nextPosition '>' (x, y) = (x + 1, y)
nextPosition '<' (x, y) = (x -1, y)
nextPosition _   (x, y) = (x, y)

housesVisited :: String -> Int
housesVisited input = Set.size $ housesVisitedInt input


housesVisitedInt :: String -> Set (Int, Int)
housesVisitedInt input = visitH (Set.singleton (0,0)) (0,0) input
  where
      visitH visitedSoFar currentPosition [] = visitedSoFar
      visitH visitedSoFar currentPosition (x:xs) = visitH (Set.insert nxt visitedSoFar) nxt xs
        where nxt = nextPosition x currentPosition

splitInput :: String -> (String, String)
splitInput input = splitA input ("", "")
  where splitA []       (existingA, existingB) = (reverse existingA, reverse existingB)
        splitA (x : xs) (existingA, existingB) = splitB xs (x : existingA, existingB)
        splitB []       (existingA, existingB) = (reverse existingA, reverse existingB)
        splitB (x : xs) (existingA, existingB) = splitA xs (existingA, x : existingB)

housesVisitedWithRoboSanta :: String -> Int
housesVisitedWithRoboSanta input = Set.size housesVisitedTSet
    where
      (santaInput, roboInput) = splitInput input
      housesVisitedTSet = Set.union (housesVisitedInt santaInput) (housesVisitedInt roboInput)

