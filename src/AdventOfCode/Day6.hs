{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day6 (
    parseInstructions
    ,gridUpdates
    , turnedOn
    , gridUpdatesLumen
    , totalBrightness
    ) where
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Control.Applicative

data Point = Point Int Int deriving (Show, Eq, Ord)
data Rectangle = Rectangle Point Point deriving (Show, Eq, Ord)
data Instruction = Toggle Rectangle | TurnOn Rectangle | TurnOff Rectangle deriving (Show, Eq, Ord)

parsePoint :: Parser Point
parsePoint = do
  x <- decimal
  _ <- char ','
  y <- decimal
  return $ Point x y


parseRectangle :: Parser Rectangle
parseRectangle = do
  lwrLeftCorner <- parsePoint
  _ <- string " through "
  upperRightCorner <- parsePoint
  _ <- skipSpace
  return $ Rectangle lwrLeftCorner upperRightCorner

toInstruction :: Text.Text -> Rectangle -> Instruction
toInstruction "turn off" rect = TurnOff rect
toInstruction "turn on" rect  = TurnOn rect
toInstruction "toggle" rect   = Toggle rect

-- turn off 301,3 through 808,453
-- turn on 351,678 through 951,908
-- toggle 720,196 through 897,994
instructionParser :: Parser Instruction
instructionParser = do
 tpe <- string "turn off" <|> string "turn on" <|> string "toggle"
 _ <- string " "
 rect <- parseRectangle
 return $ toInstruction tpe rect

instructionsParser :: Parser [Instruction]
instructionsParser = many $ instructionParser

parseInstructions :: String -> Either String [Instruction]
parseInstructions input = parseOnly instructionsParser $ Text.pack input


-- Lights in your grid are numbered from 0 to 999 in each direction;
-- the lights at each corner are at 0,0, 0,999, 999,999, and 999,0.
-- The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs.
-- Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2
-- therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

type Grid a = [[a]]

transformPosition :: Grid a -> ((Int, Int, a) -> a) -> Grid a
transformPosition grid fn = fmap rowTransformer (zip [0..] grid)
  where rowTransformer (x, row) = fmap (colTransformer x) (zip [0..] row)
        colTransformer x (y, t)   = fn (x, y, t)

updateGridPass :: Grid a -> Rectangle -> (a -> a) -> Grid a
updateGridPass grid rect fn = transformPosition grid transformer
  where matchesPt (Rectangle (Point lwrX lwrY) (Point upperX upperY)) x y
         | x >= lwrX && x <= upperX && y >= lwrY && y <= upperY = (\t -> fn t)
         | otherwise                                            = (\t -> t)
        matchRect = (matchesPt rect)
        transformer (x, y, t) = (matchRect x y) t


updateGrid :: Grid Bool -> Instruction -> Grid Bool
updateGrid grid (TurnOn rect)  = updateGridPass grid rect (\_ -> True)
updateGrid grid (TurnOff rect) = updateGridPass grid rect (\_ -> False)
updateGrid grid (Toggle rect)  = updateGridPass grid rect (\c -> not c)

gridUpdates :: Grid Bool -> [Instruction] -> Grid Bool
gridUpdates grid instructions = foldr (\ins g -> updateGrid g ins) grid instructions

innerTurnedOn :: [Bool] -> Int
innerTurnedOn row = length (filter (\a -> a) row)

turnedOn :: Grid Bool -> Int
turnedOn grid = sum $ fmap innerTurnedOn grid

-- The phrase turn on actually means that you should increase the brightness of those lights by 1.
-- The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.
-- The phrase toggle actually means that you should increase the brightness of those lights by 2.

updateGridLumen :: Grid Int -> Instruction -> Grid Int
updateGridLumen grid (TurnOn rect)  = updateGridPass grid rect (\c -> c + 1)
updateGridLumen grid (TurnOff rect) = updateGridPass grid rect (\c -> if c > 0 then c - 1 else 0)
updateGridLumen grid (Toggle rect)  = updateGridPass grid rect (\c -> c + 2)

gridUpdatesLumen :: Grid Int -> [Instruction] -> Grid Int
gridUpdatesLumen grid instructions = foldr (\ins g -> updateGridLumen g ins) grid instructions

totalBrightness :: Grid Int -> Int
totalBrightness grid = sum $ fmap sum grid
