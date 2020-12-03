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

import Control.Monad.State (execState, State, MonadState, gets)
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
    ( Optic, Is, A_Setter
    , noPrefixFieldLabels
    , makeFieldLabelsWith
    , view, at, (%)
    )
import Optics.State.Operators ((%=), (.=), (?=))


data ParamMode = Position | Immediate | Relative
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data IntcodeState a = MkIntcodeState
    { instrPtr :: Int
    , relBase :: Int
    , inputs :: [a]
    , outputs :: [a]
    , program :: IM.IntMap a
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

evalIntcodeProg :: (Integral a, Show a, Read a) => [a] -> IM.IntMap a -> [a]
evalIntcodeProg input prog = fst $ runIntcodeProg input prog

execIntcodeProg
    :: (Integral a, Show a, Read a) => [a] -> IM.IntMap a -> IM.IntMap a
execIntcodeProg input prog = snd $ runIntcodeProg input prog

runIntcodeProg
    :: (Integral a, Show a, Read a)
    => [a]
    -> IM.IntMap a
    -> ([a], IM.IntMap a)
runIntcodeProg input prog = (view #outputs res, view #program res)
  where
    res = execState interpretIntcodeProg $ MkIntcodeState
        { instrPtr = 0
        , relBase = 0
        , inputs = input
        , outputs = []
        , program = prog
        }

readPosAbs :: Integral a => Int -> State (IntcodeState a) a
readPosAbs i = do
    prog <- gets (view #program)
    pure $ IM.findWithDefault 0 i prog

readPosRelToIPtr :: Integral a => Int -> State (IntcodeState a) a
readPosRelToIPtr i = do
    iPtr <- gets (view #instrPtr)
    readPosAbs (iPtr + i)

writePos :: Int -> a -> State (IntcodeState a) ()
writePos i val = #program % at i ?= val

interpretIntcodeProg
    :: (Integral a, Show a, Read a) => State (IntcodeState a) ()
interpretIntcodeProg = do
    (opCode, paramModes) <- do
        iPtr <- gets (view #instrPtr)
        prog <- gets (view #program)
        pure $ getOpcodeAndParamModes (prog IM.! iPtr)

    let readFromParam i = case indexDefault Position paramModes (i - 1) of
            Immediate -> readPosRelToIPtr i
            Position -> do
                j <- readPosRelToIPtr i
                readPosAbs $ fromIntegral j
            Relative -> do
                rel <- gets (view #relBase)
                j <- readPosRelToIPtr i
                readPosAbs (fromIntegral j + rel)

        readToParam i = fmap fromIntegral $
            case indexDefault Position paramModes (i - 1) of
                Immediate -> error
                    "interpretIntcodeProg: encountered write parameter in \
                    \immediate-mode"
                Position -> readPosRelToIPtr i
                Relative -> do
                    rel <- gets (view #relBase)
                    j <- readPosRelToIPtr i
                    pure (j + fromIntegral rel)

    case opCode of
        n | n `elem` [1, 2] -> do
            let op = if n == 1 then (+) else (*)
            x <- op <$> readFromParam 1 <*> readFromParam 2
            dest <- readToParam 3
            writePos dest x
            #instrPtr += 4
            interpretIntcodeProg

        3 -> do
            ~(i : is) <- gets (view #inputs)
            dest <- readToParam 1
            writePos dest i
            #inputs .= is
            #instrPtr += 2
            interpretIntcodeProg

        4 -> do
            x <- readFromParam 1
            #outputs %= cons x
            #instrPtr += 2
            interpretIntcodeProg

        n | n `elem` [5, 6] -> do
            zero <- (== 0) <$> readFromParam 1
            if zero `xor` (n == 6)
                then #instrPtr += 3
                else do
                    ptr <- readFromParam 2
                    #instrPtr .= fromIntegral ptr
            interpretIntcodeProg

        n | n `elem` [7, 8] -> do
            let op = if n == 7 then (<) else (==)
                runOp x y = if x `op` y then 1 else 0
            b <- runOp <$> readFromParam 1 <*> readFromParam 2
            dest <- readToParam 3
            writePos dest b
            #instrPtr += 4
            interpretIntcodeProg

        9 -> do
            rel <- readFromParam 1
            #relBase += fromIntegral rel
            #instrPtr += 2
            interpretIntcodeProg

        99 -> #outputs %= reverse

        _ -> error $
            [Printf.s|interpretIntcodeProg: unexpected opcode %i|] opCode

getOpcodeAndParamModes
    :: (Integral a, Show a, Read a) => a -> (a, [ParamMode])
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
            [Printf.s|getOpcodeAndParamModes: unexpected param mode %i|] p

getDigits :: (Integral a, Show a) => a -> [Int]
getDigits = show .> fmap digitToInt

parseDigits :: (Integral a, Read a) => [Int] -> a
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

parseInput :: Integral a => T.Text -> IM.IntMap a
parseInput = T.splitOn "," .> fmap readInt .> zip [0..] .> IM.fromList
  where
    readInt txt = case TR.signed TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
