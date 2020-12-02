module Day01 where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))


main :: IO ()
main = do
    input <- parseInput <$> readUtf8File "day-01/input.txt"
    print $ part1 input
    print $ part2 input

part1 :: [Int] -> Int
part1 = fmap fuelRequiredForMass .> sum
  where
    fuelRequiredForMass :: Int -> Int
    fuelRequiredForMass m = (m `div` 3) - 2

part2 :: [Int] -> Int
part2 = fmap fuelRequiredForMass .> sum
  where
    fuelRequiredForMass :: Int -> Int
    fuelRequiredForMass m
        | fuel <= 0 = 0
        | otherwise = fuel + fuelRequiredForMass fuel
      where
        fuel = (m `div` 3) - 2

readUtf8File :: FilePath -> IO T.Text
readUtf8File path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> [Int]
parseInput = T.lines .> fmap readInt
  where
    readInt :: T.Text -> Int
    readInt txt = case TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
