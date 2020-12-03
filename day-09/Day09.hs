{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Day09 where

import Control.Monad.State (execState, State, MonadState, gets, evalState)
import Data.Bifunctor (first)
import Data.ByteString qualified as B
import Data.Char (intToDigit, digitToInt)
import Data.IntMap qualified as IM
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf
import Optics
    ((^.),  Optic, Is, A_Setter
    , noPrefixFieldLabels
    , makeFieldLabelsWith
    , view, at, (%)
    )
import Optics.State.Operators ((%=), (.=), (?=))


data ParamMode = Position | Immediate | Relative
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data IntcodeState = MkIntcodeState
    { instrPtr :: Int
    , relBase :: Int
    , inputs :: [Integer]
    , outputs :: [Integer]
    , program :: IM.IntMap Integer
    } deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''IntcodeState

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-09/input.txt"
    print $ part1 input

part1 :: IM.IntMap Integer -> Integer
part1 prog = case evalIntcodeProg [1] prog of
    [x] -> x
    out -> error $ [Printf.s|part1: bad output: %?|] out

evalIntcodeProg :: [Integer] -> IM.IntMap Integer -> [Integer]
evalIntcodeProg input prog = fst $ runIntcodeProg input prog

execIntcodeProg :: [Integer] -> IM.IntMap Integer -> IM.IntMap Integer
execIntcodeProg input prog = snd $ runIntcodeProg input prog

runIntcodeProg :: [Integer] -> IM.IntMap Integer -> ([Integer], IM.IntMap Integer)
runIntcodeProg input prog = (view #outputs res, view #program res)
  where
    res = execState interpretIntcodeProg $ MkIntcodeState
        { instrPtr = 0
        , relBase = 0
        , inputs = input
        , outputs = []
        , program = prog
        }

readPosAbs :: Int -> State IntcodeState Integer
readPosAbs i = do
    prog <- gets (view #program)
    pure $ IM.findWithDefault 0 i prog

readPosRelToIPtr :: Int -> State IntcodeState Integer
readPosRelToIPtr i = do
    iPtr <- gets (view #instrPtr)
    readPosAbs (iPtr + i)

writePos :: Int -> Integer -> State IntcodeState ()
writePos i val = #program % at i ?= val

interpretIntcodeProg :: State IntcodeState ()
interpretIntcodeProg = do
    (opCode, paramModes) <- do
        iPtr <- gets (view #instrPtr)
        prog <- gets (view #program)
        pure $ getOpcodeAndParamModes (prog IM.! iPtr)

    let readParam i = case indexDefault Position paramModes (i - 1) of
            Immediate -> readPosRelToIPtr i
            Position -> do
                j <- readPosRelToIPtr i
                readPosAbs $ fromInteger j
            Relative -> do
                rel <- gets (view #relBase)
                j <- readPosRelToIPtr i
                readPosAbs (fromInteger j + rel)

    case opCode of
        n | n `elem` [1, 2] -> do
            let op = if n == 1 then (+) else (*)
            x <- op <$> readParam 1 <*> readParam 2
            dest <- readPosRelToIPtr 3
            writePos (fromInteger dest) x
            #instrPtr += 4
            interpretIntcodeProg

        3 -> do
            ~(i : is) <- gets (view #inputs)
            dest <- readPosRelToIPtr 1
            writePos (fromInteger dest) i
            #inputs .= is
            #instrPtr += 2
            interpretIntcodeProg

        4 -> do
            x <- readParam 1
            #outputs %= cons x
            #instrPtr += 2
            interpretIntcodeProg

        n | n `elem` [5, 6] -> do
            zero <- (== 0) <$> readParam 1
            if zero `xor` (n == 6)
                then #instrPtr += 3
                else do
                    ptr <- readParam 2
                    #instrPtr .= fromInteger ptr
            interpretIntcodeProg

        n | n `elem` [7, 8] -> do
            let op = if n == 7 then (<) else (==)
                runOp x y = if x `op` y then 1 else 0
            b <- runOp <$> readParam 1 <*> readParam 2
            dest <- readPosRelToIPtr 3
            writePos (fromInteger dest) b
            #instrPtr += 4
            interpretIntcodeProg

        9 -> do
            rel <- readParam 1
            #relBase += fromInteger rel
            #instrPtr += 2
            interpretIntcodeProg

        99 -> #outputs %= reverse

        _ -> error $
            [Printf.s|interpretIntcodeProg: unexpected opcode %i|] opCode

getOpcodeAndParamModes :: Integer -> (Integer, [ParamMode])
getOpcodeAndParamModes instr =
    (parseDigits opCode, fmap mkParamMode paramModes)
  where
    (opCode, paramModes) =
        first reverse $ splitAt 2 $ reverse $ getDigits instr
    mkParamMode p
        | p == 0 = Position
        | p == 1 = Immediate
        | p == 2 = Relative
        | otherwise = error $
            [Printf.s|getOpcodeAndParamModes:
  while parsing instruction %i:
    encountered unexpected param mode %i|]
                instr p

getDigits :: Integer -> [Int]
getDigits = show .> fmap digitToInt

parseDigits :: [Int] -> Integer
parseDigits = fmap intToDigit .> read

indexDefault :: a -> [a] -> Int -> a
indexDefault def list n = case list of
    [] -> def
    x:xs -> if n == 0 then x else indexDefault def xs (n - 1)

cons :: a -> [a] -> [a]
cons = (:)

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

(+=)
    :: (Is k A_Setter, MonadState s m, Num a)
    => Optic k is s s a a
    -> a
    -> m ()
optic += x = optic %= (+ x)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> IM.IntMap Integer
parseInput = T.splitOn "," .> fmap readInt .> zip [0..] .> IM.fromList
  where
    readInt :: T.Text -> Integer
    readInt txt = case TR.signed TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
