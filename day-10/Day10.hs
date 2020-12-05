{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}

module Day10 where

import Data.ByteString qualified as B
import Data.Maybe (listToMaybe)
import Data.Set qualified as S
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf
import Control.Applicative (liftA2)
import Control.Arrow ((&&&), second)
import Data.Ord (comparing)


data Tile = Empty | Asteroid
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data V2 a = MkV2 !a !a
    deriving (Bounded, Eq, Functor, Foldable, Ord, Read, Show)

instance Applicative V2 where
    pure x = MkV2 x x
    MkV2 f1 f2 <*> MkV2 x1 x2 = MkV2 (f1 x1) (f2 x2)

instance Num a => Num (V2 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = fromInteger .> pure

type Grid a = V.Vector (V.Vector a)

data Visibility = Unknown | Visible | Invisible | Obscured
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-10/input.txt"
    print $ part1 input

part1 :: Grid Tile -> Int
part1 = bestIndex .> snd

bestIndex :: Grid Tile -> (V2 Int, Int)
bestIndex =
    allMatchingIndicesRadiatingOut Asteroid
        .> S.toList
        .> L.maximumBy (comparing $ snd .> S.size)
        .> second S.size

allMatchingIndicesRadiatingOut
    :: Eq a => a -> Grid a -> S.Set (V2 Int, S.Set (V2 Int))
allMatchingIndicesRadiatingOut val grid =
    S.map (id &&& firstMatchesRadiatingOut val grid) matchingIndices
  where
    matchingIndices = S.filter (\pt -> index grid pt == val) $ allIndices grid

firstMatchesRadiatingOut :: Eq a => a -> Grid a -> V2 Int -> S.Set (V2 Int)
firstMatchesRadiatingOut val grid base = go initGrid
  where
    initVal x = if x == val then Unknown else Invisible
    initGrid = setIndex base Invisible $ fmap (fmap initVal) grid
    isVal pt = index grid pt == val
    block pt f = setIndex pt Obscured .> f
    go gridVis = case firstIndexOf Unknown gridVis of
        Just pt ->
          let
            ray = drop 1 $
                takeWhile (inBounds gridVis) $ pointsFromInDirOf base pt
            updateGrid = case dropWhile (isVal .> not) ray of
                vis : blocked ->
                    foldr block (setIndex vis Visible) (filter isVal blocked)
                [] -> error "firstMatchesRadiatingOut: impossible"
          in
            go (updateGrid gridVis)
        Nothing -> indices Visible gridVis

pointsFromInDirOf :: V2 Int -> V2 Int -> [V2 Int]
pointsFromInDirOf p1@(MkV2 x1 y1) p2@(MkV2 x2 y2)
    | p1 == p2 = []
    | otherwise = do
        i <- [0..]
        pure $ p1 + pure i * step
  where
    xDiff = abs $ x2 - x1
    yDiff = abs $ y2 - y1
    count = gcd xDiff yDiff
    step = fmap (`div` count) (p2 - p1)

indices :: Eq a => a -> Grid a -> S.Set (V2 Int)
indices val grid = S.fromList $ V.toList $ do
    (y, vec) <- V.indexed grid
    x <- V.elemIndices val vec
    pure $ MkV2 x y

firstIndexOf :: Eq a => a -> Grid a -> Maybe (V2 Int)
firstIndexOf val grid = listToMaybe $ V.toList $ do
    (y, vec) <- V.indexed grid
    x <- V.elemIndices val vec
    pure $ MkV2 x y

allIndices :: Grid a -> S.Set (V2 Int)
allIndices grid = S.fromList $ do
    x <- [0 .. rows-1]
    y <- [0 .. cols-1]
    pure $ MkV2 x y
  where
    MkV2 cols rows = dimen grid

inBounds :: Grid a -> V2 Int -> Bool
inBounds grid (MkV2 x y) = between 0 (cols - 1) x && between 0 (rows - 1) y
  where
    MkV2 cols rows = dimen grid

dimen :: Grid a -> V2 Int
dimen grid = MkV2 cols rows
  where
    rows = V.length grid
    cols = if rows == 0 then 0 else V.length (V.head grid)

index :: Grid a -> V2 Int -> a
index grid (MkV2 x y) = grid V.! y V.! x

setIndex :: V2 Int -> a -> Grid a -> Grid a
setIndex (MkV2 x y) val grid = grid V.// [(y, (grid V.! y) V.// [(x, val)])]

between :: Ord a => a -> a -> a -> Bool
between a b x = (a <= x) && (x <= b)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> Grid Tile
parseInput =
    T.lines
        .> V.fromList
        .> fmap (T.unpack .> V.fromList .> fmap charToTile)
  where
    charToTile c = case c of
        '.' -> Empty
        '#' -> Asteroid
        _ -> error $ [Printf.s|parseInput: char '%c' not in {'.', '#'}|] c
