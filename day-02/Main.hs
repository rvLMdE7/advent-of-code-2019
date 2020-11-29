{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Vector qualified as V
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))


main :: IO ()
main = do
    input <- parseInput <$> readUtf8File "input.txt"
    print $ part1 input

part1 :: V.Vector Int -> Int
part1 prog = runIntcodeProg 0 (prog V.// [(1, 12), (2, 2)]) V.! 0

runIntcodeProg :: Int -> V.Vector Int -> V.Vector Int
runIntcodeProg iPtr prog = case prog V.! iPtr of
    1 ->
      let
        xSrc = prog V.! (iPtr + 1)
        ySrc = prog V.! (iPtr + 2)
        x = prog V.! xSrc
        y = prog V.! ySrc
        dest = prog V.! (iPtr + 3)
        newProg = prog V.// [(dest, x + y)]
      in
        runIntcodeProg (iPtr + 4) newProg
    2 ->
      let
        xSrc = prog V.! (iPtr + 1)
        ySrc = prog V.! (iPtr + 2)
        x = prog V.! xSrc
        y = prog V.! ySrc
        dest = prog V.! (iPtr + 3)
        newProg = prog V.// [(dest, x * y)]
      in
        runIntcodeProg (iPtr + 4) newProg
    99 -> prog

readUtf8File :: FilePath -> IO T.Text
readUtf8File path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> V.Vector Int
parseInput = T.splitOn "," .> fmap readInt .> V.fromList
  where
    readInt :: T.Text -> Int
    readInt txt = case TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
