{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Day11 where

import Control.Monad.State (State, MonadState, execState)
import Data.Bifunctor (bimap)
import Data.Bits (xor)
import Data.ByteString qualified as B
import Data.Char (intToDigit, digitToInt)
import Data.Foldable qualified as F
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Read qualified as TR
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf
import Optics
    ( Optic, Is, A_Setter, (%), noPrefixFieldLabels, makeFieldLabelsWith, at
    , zoom, view )
import Optics.State (use)
import Optics.State.Operators ((<%=), (%=), (.=), (?=))


data ParamMode = Position | Immediate | Relative
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Direc = L | R
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Orientation = North | East | South | West
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Intcode = MkIntcode
    { instrPtr :: Int
    , relBase :: Int
    , inputs :: Seq Int
    , outputs :: Seq Int
    , program :: IM.IntMap Int
    , stillRunning :: Bool
    , pauseOnMissingInput :: Bool
    } deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Intcode

data PanelCol = Black | White
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Robot = MkRobot
    { intcode :: Intcode
    , position :: (Int, Int)
    , hullGrid :: M.Map (Int, Int) PanelCol
    , orientation :: Orientation
    , trail :: Seq (Int, Int)
    } deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Robot

main :: IO ()
main = do
    prog <- parseInput <$> readFileUtf8 "day-11/input.txt"
    print $ part1 prog

part1 :: IM.IntMap Int -> Int
part1 = runRobot .> view #trail .> drop1End .> seqToSet .> Set.size
  where
    seqToSet = F.toList .> Set.fromList
    drop1End xs = Seq.deleteAt (Seq.length xs - 1) xs

runRobot :: IM.IntMap Int -> Robot
runRobot prog = execState interpRobot $ MkRobot
    { intcode = MkIntcode
        { instrPtr = 0
        , relBase = 0
        , inputs = Seq.empty
        , outputs = Seq.empty
        , program = prog
        , stillRunning = True
        , pauseOnMissingInput = True
        }
    , position = (0, 0)
    , hullGrid = M.empty
    , orientation = North
    , trail = Seq.singleton (0, 0)
    }

interpRobot :: State Robot ()
interpRobot = use (#intcode % #stillRunning) >>= \case
    False -> pure ()
    True -> do
        pos <- use #position
        colIn <- fromMaybe Black <$> use (#hullGrid % at pos)
        out <- zoom #intcode $ do
            #inputs %= (fromCol colIn :<|)
            interpIntcode
            use #outputs
        case out of
            front :|> colOut :|> dir  -> do
                #hullGrid % at pos ?= toCol colOut
                orient <- #orientation <%= turn (toDir dir)
                newPos <- #position <%= move orient
                #trail %= (:|> newPos)
                #intcode % #outputs .= front
            _ -> error $ [Printf.s|interpRobot: not enough outputs: %?|] out
        interpRobot
  where
    fromCol col = if col == Black then 0 else 1
    toCol n
        | n == 0 = Black
        | n == 1 = White
        | otherwise = error $ [Printf.s|interpRobot: bad output colour '%i'|] n
    toDir n
        | n == 0 = L
        | n == 1 = R
        | otherwise = error $ [Printf.s|interpRobot: bad output direc '%i'|] n

interpIntcode :: State Intcode ()
interpIntcode = do
    (opCode, parModes) <- getInstr

    let readPar = readParWith parModes
        writePar = writeParWith parModes

    case opCode of
        n | n `elem` [1, 2] -> do
            let op = if n == 1 then (+) else (*)
            val <- op <$> readPar 1 <*> readPar 2
            writePar 3 val
            #instrPtr += 4
            interpIntcode

        3 -> use #inputs >>= \case
            inp :<| inps -> do
                writePar 1 inp
                #inputs .= inps
                #instrPtr += 2
                interpIntcode
            Seq.Empty -> use #pauseOnMissingInput >>= \case
                True -> pure ()
                False -> error "interpIntcode: no input"

        4 -> do
            val <- readPar 1
            #outputs %= (:|> val)
            #instrPtr += 2
            interpIntcode

        n | n `elem` [5, 6] -> do
            val <- readPar 1
            if (val == 0) `xor` (n == 6)
                then #instrPtr += 3
                else do
                    ptr <- readPar 2
                    #instrPtr .= ptr
            interpIntcode

        n | n `elem` [7, 8] -> do
            let op = if n == 7 then (<) else (==)
                comp x y = if x `op` y then 1 else 0
            val <- comp <$> readPar 1 <*> readPar 2
            writePar 3 val
            #instrPtr += 4
            interpIntcode

        9 -> do
            rel <- readPar 1
            #relBase += rel
            #instrPtr += 2
            interpIntcode

        99 -> #stillRunning .= False

        _ -> error $ [Printf.s|interpIntcode: unexpected opcode %i|] opCode

getInstr :: State Intcode (Int, [ParamMode])
getInstr = do
    iPtr <- use #instrPtr
    prog <- use #program
    pure $ getOpcodeParModes (prog IM.! iPtr)

readPosAbs :: Int -> State Intcode Int
readPosAbs i = IM.findWithDefault 0 i <$> use #program

readPosRelToIPtr :: Int -> State Intcode Int
readPosRelToIPtr i = do
    iPtr <- use #instrPtr
    readPosAbs (iPtr + i)

readParWith :: [ParamMode] -> Int -> State Intcode Int
readParWith parModes i = case indexDef Position parModes (i - 1) of
    Immediate -> readPosRelToIPtr i
    Position -> readPosRelToIPtr i >>= readPosAbs
    Relative -> do
        rel <- use #relBase
        j <- readPosRelToIPtr i
        readPosAbs (j + rel)

writeParWith :: [ParamMode] -> Int -> Int -> State Intcode ()
writeParWith parModes i val = do
    dest <- case indexDef Position parModes (i - 1) of
        Immediate -> error
            "interpIntcode: encountered write parameter in immediate-mode"
        Position -> readPosRelToIPtr i
        Relative -> do
            rel <- use #relBase
            j <- readPosRelToIPtr i
            pure (j + rel)
    #program % at dest ?= val

getOpcodeParModes :: Int -> (Int, [ParamMode])
getOpcodeParModes =
    getDigits
        .> reverse
        .> splitAt 2
        .> bimap (reverse .> putDigits) (fmap mkParMode)
  where
    mkParMode par
        | par == 0 = Position
        | par == 1 = Immediate
        | par == 2 = Relative
        | otherwise = error $
            [Printf.s|getOpcodeParModes: unexpected param mode %i|] par

turn :: Direc -> Orientation -> Orientation
turn = \case
    L -> nextAntiClockwise
    R -> nextClockwise

nextClockwise :: Orientation -> Orientation
nextClockwise = \case
    North -> East
    East -> South
    South -> West
    West -> North

nextAntiClockwise :: Orientation -> Orientation
nextAntiClockwise = \case
    North -> West
    East -> North
    South -> East
    West -> South

move :: Orientation -> (Int, Int) -> (Int, Int)
move orient = addPt (orientToVec orient)

orientToVec :: Orientation -> (Int, Int)
orientToVec = \case
    North -> (0, 1)
    East -> (1, 0)
    South -> (0, -1)
    West -> (-1, 0)

addPt :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPt (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getDigits :: (Integral a, Show a) => a -> [Int]
getDigits = show .> fmap digitToInt

putDigits :: (Integral a, Read a) => [Int] -> a
putDigits = fmap intToDigit .> read

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> IM.IntMap Int
parseInput = T.splitOn "," .> fmap readInt .> zip [0..] .> IM.fromList
  where
    readInt txt = case TR.signed TR.decimal txt of
        Right (n, _) -> n
        Left _ -> error $
            [Printf.s|parseInput: '%q' not an Int|] (TL.fromStrict txt)

indexDef :: a -> [a] -> Int -> a
indexDef def list n = case list of
    [] -> def
    x : xs -> if n == 0 then x else indexDef def xs (n - 1)

(+=)
    :: (Is k A_Setter, MonadState s m, Num a)
    => Optic k is s s a a
    -> a
    -> m ()
optic += x = optic %= (+ x)
