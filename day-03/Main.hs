{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))


data Direc = L | R | U | D
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

main :: IO ()
main = do
    input <- parseInput <$> readUtf8File "input.txt"
    print $ uncurry part1 input

part1 :: [(Direc, Int)] -> [(Direc, Int)] -> Int
part1 one two = undefined

readUtf8File :: FilePath -> IO T.Text
readUtf8File path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> ([(Direc, Int)], [(Direc, Int)])
parseInput txt = case T.lines txt of
    [one, two] -> (parseLine one, parseLine two)
  where
    parseLine :: T.Text -> [(Direc, Int)]
    parseLine = T.splitOn "," .> fmap readArc

    readArc :: T.Text -> (Direc, Int)
    readArc txt = (dir, dist)
      where
        dir = read [T.head txt]
        dist = case TR.decimal (T.tail txt) of
            Left _ -> error "parseInput: input not an Int"
            Right (n, _) -> n
