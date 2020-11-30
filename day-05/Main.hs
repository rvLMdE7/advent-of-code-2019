{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Bifunctor (first)
import Data.Bool (bool)
import Control.Monad.ST (ST, runST)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))
import Language.Haskell.Printf qualified as Pr


data ParamMode = Position | Immediate
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

main :: IO ()
main = do
    input <- parseInput <$> readUtf8File "input.txt"
    print $ part1 input
    print $ part2 input

part1 :: V.Vector Int -> Int
part1 prog = last $ evalIntcodeProg [1] prog

part2 :: V.Vector Int -> Int
part2 prog = last $ evalIntcodeProg [5] prog

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
interpretIntcodeProg iPtr' inputs' prog' =
    reverse <$> go iPtr' inputs' [] prog'
  where
    go :: Int -> [Int] -> [Int] -> VM.MVector s Int -> ST s [Int]
    go iPtr inputs outputs prog = do
        (opCode, paramModes) <- getOpcodeAndParamModes <$> VM.read prog iPtr

        let readProgOffset :: Int -> ST s Int
            readProgOffset i = VM.read prog (iPtr + i)

            readParam :: Int -> ST s Int
            readParam i = case indexDefault Position paramModes (i - 1) of
                Immediate -> readProgOffset i
                Position -> do
                    j <- readProgOffset i
                    VM.read prog j

        case opCode of
            n | n `elem` [1, 2] -> do
                let op = if n == 1 then (+) else (*)
                x <- op <$> readParam 1 <*> readParam 2
                dest <- readProgOffset 3
                VM.write prog dest x
                go (iPtr + 4) inputs outputs prog

            3 -> do
                let input : otherInputs = inputs
                dest <- readProgOffset 1
                VM.write prog dest input
                go (iPtr + 2) otherInputs outputs prog

            4 -> do
                x <- readParam 1
                go (iPtr + 2) inputs (x : outputs) prog

            n | n `elem` [5, 6] -> do
                let decideJump = if n == 5 then bool else flip bool
                cond <- (== 0) <$> readParam 1
                ptr <- readParam 2
                let nextPtr = decideJump ptr (iPtr + 3) cond
                go nextPtr inputs outputs prog

            n | n `elem` [7, 8] -> do
                let op = if n == 7 then (<) else (==)
                x <- readParam 1
                y <- readParam 2
                let result = if x `op` y then 1 else 0
                dest <- readProgOffset 3
                VM.write prog dest result
                go (iPtr + 4) inputs outputs prog

            99 -> pure outputs

            n -> do
                finalProg <- V.freeze prog
                let errMsg = [Pr.s|interpretIntcodeProg:
  opcode = %?
  instr-ptr = %?
  inputs = %?
  outputs = %?
  prog = %?|]
                        n iPtr inputs outputs finalProg
                error errMsg

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
