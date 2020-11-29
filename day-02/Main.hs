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
part1 prog = runIntcodeProg 0 (prog V.// [(1, 12), (2, 2)])

runIntcodeProg :: Int -> V.Vector Int -> Int
runIntcodeProg iPtr prog = case prog V.! iPtr of
    1 ->
      let
        x = prog V.! (iPtr + 1)
        y = prog V.! (iPtr + 2)
      in
        runIntcodeProg (iPtr + 4) (prog V.// [(iPtr + 3, x + y)])
    2 ->
      let
        x = prog V.! (iPtr + 1)
        y = prog V.! (iPtr + 2)
      in
        runIntcodeProg (iPtr + 4) (prog V.// [(iPtr + 3, x * y)])
    99 -> prog V.! 0

readUtf8File :: FilePath -> IO T.Text
readUtf8File path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> V.Vector Int
parseInput = T.splitOn "," .> fmap readInt .> V.fromList
  where
    readInt :: T.Text -> Int
    readInt txt = case TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
