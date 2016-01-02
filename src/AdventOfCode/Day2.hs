{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day2 (
    Box(..),
    wrappingPaper,
    parseBoxes,
    ribbonLength
    ) where
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Control.Applicative

data Box = Box Int Int Int deriving (Show, Eq, Ord)

wrappingPaper :: Box -> Int
wrappingPaper (Box w l h) = 2*l*w + 2*w*h + 2*h*l + extra
  where extra = minimum [s1, s2, s3]
        s1    = w * l
        s2    = w * h
        s3    = l * h

boxVolume :: Box -> Int
boxVolume (Box w l h) = w * l * h

smallestFacePerimeter :: Box -> Int
smallestFacePerimeter (Box w l h) = minimum [p1, p2, p3]
  where p1 = w*2 + l*2
        p2 = w*2 + h*2
        p3 = h*2 + l*2

ribbonLength :: Box -> Int
ribbonLength box = (smallestFacePerimeter box) + (boxVolume box)

parseBox :: Parser Box
parseBox = do
  w <- decimal
  _ <- char 'x'
  l <- decimal
  _ <- char 'x'
  h <- decimal
  _ <- skipSpace
  return $ Box w l h

boxesParser :: Parser [Box]
boxesParser = many $ parseBox

parseBoxes :: String -> Either String [Box]
parseBoxes input = parseOnly boxesParser $ Text.pack input

