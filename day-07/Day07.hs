{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day07 where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.ByteString qualified as B
import Data.List qualified as L
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Flow ((.>))
import Language.Haskell.Printf qualified as Pr


data ParamMode = Position | Immediate
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data V5 a = MkV5 !a !a !a !a !a
    deriving (Eq, Functor, Foldable, Ord, Read, Show)

instance Applicative V5 where
    pure x = MkV5 x x x x x
    MkV5 f1 f2 f3 f4 f5 <*> MkV5 x1 x2 x3 x4 x5 =
        MkV5 (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-07/input.txt"
    print $ part1 input

part1 :: V.Vector Int -> Int
part1 prog = snd $ maxLinearThrust prog

maxLinearThrust :: V.Vector Int -> (V5 Int, Int)
maxLinearThrust prog = L.maximumBy (comparing snd) $ do
    [phase1, phase2, phase3, phase4, phase5] <- L.permutations [0..4]
    let phases = MkV5 phase1 phase2 phase3 phase4 phase5
    pure (phases, last $ evalSameLinearAmplifiers phases [0] prog)

evalSameLinearAmplifiers :: V5 Int -> [Int] -> V.Vector Int -> [Int]
evalSameLinearAmplifiers phases input program =
    fst $ runSameLinearAmplifiers phases input program

runSameLinearAmplifiers
    :: V5 Int -> [Int] -> V.Vector Int -> ([Int], V5 (V.Vector Int))
runSameLinearAmplifiers phases input program =
    runLinearAmplifiers phases input (pure program)

runLinearAmplifiers
    :: V5 Int -> [Int] -> V5 (V.Vector Int) -> ([Int], V5 (V.Vector Int))
runLinearAmplifiers phases input programs =
  let
    MkV5 phase1 phase2 phase3 phase4 phase5 = phases
    MkV5 prog1 prog2 prog3 prog4 prog5 = programs
    (output1, endProg1) = runIntcodeProg (phase1 : input) prog1
    (output2, endProg2) = runIntcodeProg (phase2 : output1) prog2
    (output3, endProg3) = runIntcodeProg (phase3 : output2) prog3
    (output4, endProg4) = runIntcodeProg (phase4 : output3) prog4
    (output5, endProg5) = runIntcodeProg (phase5 : output4) prog5
  in
    (output5, MkV5 endProg1 endProg2 endProg3 endProg4 endProg5)

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

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> V.Vector Int
parseInput = T.splitOn "," .> fmap readInt .> V.fromList
  where
    readInt :: T.Text -> Int
    readInt txt = case TR.signed TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
