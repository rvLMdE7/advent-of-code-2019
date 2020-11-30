{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Bifunctor (first)
import Control.Monad.ST (ST, runST)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))


data ParamMode = Position | Immediate
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

main :: IO ()
main = do
    input <- parseInput <$> readUtf8File "input.txt"
    print $ part1 input

part1 :: V.Vector Int -> Int
part1 prog = last $ evalIntcodeProg [1] prog

execIntcodeProg :: [Int] -> V.Vector Int -> V.Vector Int
execIntcodeProg inputs prog = snd $ runIntcodeProg inputs prog

evalIntcodeProg :: [Int] -> V.Vector Int -> [Int]
evalIntcodeProg inputs prog = fst $ runIntcodeProg inputs prog

runIntcodeProg :: [Int] -> V.Vector Int -> ([Int], V.Vector Int)
runIntcodeProg inputs prog = runST $ do
    mProg <- V.thaw prog
    outputs <- interpretIntcodeProg 0 inputs mProg
    finalProg <- V.freeze mProg
    pure (outputs, finalProg)

interpretIntcodeProg
    :: forall s. Int -> [Int] -> VM.MVector s Int -> ST s [Int]
interpretIntcodeProg iPtr' inputs' = go iPtr' inputs' []
  where
    go :: Int -> [Int] -> [Int] -> VM.MVector s Int -> ST s [Int]
    go iPtr inputs outputs prog = do
        instr <- VM.read prog iPtr

        let (opCode, paramModes) = getOpcodeAndParamModes instr

            readProgOffset :: Int -> ST s Int
            readProgOffset i = VM.read prog (iPtr + i)

            readParam :: Int -> ST s Int
            readParam i = case indexDefault Position paramModes i of
                Immediate -> readProgOffset i
                Position -> do
                    j <- readProgOffset i
                    VM.read prog j

        case opCode of
            n | n `elem` [1, 2] -> do
                let op = if n == 1 then (+) else (*)
                x <- op <$> readParam 0 <*> readParam 1
                dest <- readProgOffset 2
                VM.write prog dest x
                go (iPtr + 4) inputs outputs prog

            3 -> do
                let input : otherInputs = inputs
                dest <- readProgOffset 0
                VM.write prog dest input
                go (iPtr + 2) otherInputs outputs prog

            4 -> do
                x <- readParam 0
                go (iPtr + 2) inputs (x : outputs) prog

            99 -> pure outputs

getOpcodeAndParamModes :: Int -> (Int, [ParamMode])
getOpcodeAndParamModes x = (parseDigits opCode, fmap mkParamMode paramModes)
  where
    (opCode, paramModes) = first reverse $ splitAt 2 $ reverse $ getDigits x
    mkParamMode y = if y == 0 then Position else Immediate

getDigits :: Int -> [Int]
getDigits = show .> fmap (\c -> read [c])

parseDigits :: [Int] -> Int
parseDigits = fmap show .> mconcat .> read

indexDefault :: a -> [a] -> Int -> a
indexDefault x list n = case list of
    [] -> x
    y:ys -> if n == 0 then y else indexDefault x ys (n - 1)

readUtf8File :: FilePath -> IO T.Text
readUtf8File path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> V.Vector Int
parseInput = T.splitOn "," .> fmap readInt .> V.fromList
  where
    readInt :: T.Text -> Int
    readInt txt = case TR.signed TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
