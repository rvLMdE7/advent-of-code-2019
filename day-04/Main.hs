{-# LANGUAGE NumericUnderscores #-}

import Flow ((.>))
import Data.List (group)


main :: IO ()
main = do
    let input = (183_564, 657_474)
    print $ uncurry part1 input

part1 :: Int -> Int -> Int
part1 a b = length [ pw | pw <- [a..b], isPossiblePassword pw ]

isPossiblePassword :: Int -> Bool
isPossiblePassword n = hasTwoAdjacent digits && isNonDecreasing digits
  where
    digits = getDigits n

getDigits :: Int -> [Int]
getDigits = show .> fmap (\c -> read [c])

hasTwoAdjacent :: Eq a => [a] -> Bool
hasTwoAdjacent = group .> any (length .> (> 1))

isNonDecreasing :: Ord a => [a] -> Bool
isNonDecreasing list = case list of
    x:y:zs -> (x <= y) && isNonDecreasing (y:zs)
    _ -> True
