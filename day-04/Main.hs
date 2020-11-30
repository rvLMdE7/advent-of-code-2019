{-# LANGUAGE NumericUnderscores #-}

import Flow ((.>))
import Data.List (group)


main :: IO ()
main = do
    let input = (183_564, 657_474)
    print $ uncurry part1 input
    print $ uncurry part2 input

part1 :: Int -> Int -> Int
part1 a b = length [ pw | pw <- [a..b], isPossiblePassword pw ]

part2 :: Int -> Int -> Int
part2 a b = length [ pw | pw <- [a..b], isVeryPossiblePassword pw ]

isPossiblePassword :: Int -> Bool
isPossiblePassword n = hasTwoAdjacent digits && isNonDecreasing digits
  where
    digits = getDigits n

isVeryPossiblePassword :: Int -> Bool
isVeryPossiblePassword n =
    hasTwoAdjacentButNoMore digits && isNonDecreasing digits
  where
    digits = getDigits n

getDigits :: Int -> [Int]
getDigits = show .> fmap (\c -> read [c])

hasTwoAdjacent :: Eq a => [a] -> Bool
hasTwoAdjacent xs = any (> 1) grouped
  where
    grouped = length <$> group xs

hasTwoAdjacentButNoMore :: Eq a => [a] -> Bool
hasTwoAdjacentButNoMore xs = any (> 1) grouped && all (<= 2) grouped
  where
    grouped = length <$> group xs

isNonDecreasing :: Ord a => [a] -> Bool
isNonDecreasing list = case list of
    x:y:zs -> (x <= y) && isNonDecreasing (y:zs)
    _ -> True
