{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import Control.Monad (guard)
import Data.Vector qualified as V
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))


main :: IO ()
main = do
    input <- parseInput <$> readUtf8File "day-02/input.txt"
    print $ part1 input
    print $ part2 input

part1 :: V.Vector Int -> Int
part1 prog = runIntcodeProg 0 (prog V.// [(1, 12), (2, 2)]) V.! 0

part2 :: V.Vector Int -> Int
part2 prog = head $ do
    noun <- [0..99]
    verb <- [0..99]
    let result = runIntcodeProg 0 (prog V.// [(1, noun), (2, verb)]) V.! 0
    guard $ result == 19_690_720
    pure (100 * noun + verb)

runIntcodeProg :: Int -> V.Vector Int -> V.Vector Int
runIntcodeProg iPtr prog = case prog V.! iPtr of
    1 -> applyBinaryOp (+)
    2 -> applyBinaryOp (*)
    99 -> prog
  where
    applyBinaryOp op =
      let
        xSrc = prog V.! (iPtr + 1)
        ySrc = prog V.! (iPtr + 2)
        x = prog V.! xSrc
        y = prog V.! ySrc
        dest = prog V.! (iPtr + 3)
        newProg = prog V.// [(dest, x `op` y)]
      in
        runIntcodeProg (iPtr + 4) newProg

readUtf8File :: FilePath -> IO T.Text
readUtf8File path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> V.Vector Int
parseInput = T.splitOn "," .> fmap readInt .> V.fromList
  where
    readInt :: T.Text -> Int
    readInt txt = case TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
