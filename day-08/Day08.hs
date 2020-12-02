module Day08 where

import Data.Vector qualified as V
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Char (digitToInt)
import Flow ((.>))
import Data.Ord (comparing)
import Data.Bifunctor (second)


type Layer a = V.Vector (V.Vector a)

type Image a = V.Vector (Layer a)

main :: IO ()
main = do
    input <- parseInput (25, 6) <$> readFileUtf8 "day-08/input.txt"
    print $ part1 input

part1 :: Image Int -> Int
part1 image = countOccurencesLayer 1 layer * countOccurencesLayer 2 layer
  where
    i = layerWithLeast 0 image
    layer = image V.! i

layerWithLeast :: Eq a => a -> Image a -> Int
layerWithLeast x =
    V.indexed
        .> fmap (second $ countOccurencesLayer x)
        .> V.minimumBy (comparing snd)
        .> fst

countOccurencesLayer :: Eq a => a -> Layer a -> Int
countOccurencesLayer x = fmap (countOccurences x) .> sum

countOccurences :: Eq a => a -> V.Vector a -> Int
countOccurences x = V.elemIndices x .> length

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: (Int, Int) -> T.Text -> Image Int
parseInput (cols, rows) = T.strip .> mkLayers .> fmap mkLayer .> asVecs
  where
    mkLayers = T.chunksOf (cols * rows)
    mkLayer = T.chunksOf cols .> fmap (T.unpack .> fmap digitToInt)
    asVecs = V.fromList .> fmap (V.fromList .> fmap V.fromList)
