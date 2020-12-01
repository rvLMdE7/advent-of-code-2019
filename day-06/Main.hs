{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Tree qualified as Tr
import Flow ((.>))


main :: IO ()
main = do
    input <- parseInputs <$> readUtf8File "input.txt"
    pure ()

directOrbits :: a -> Tr.Tree a -> [a]
directOrbits x tree = []

makeTree :: Eq a => a -> [(a, a)] -> Tr.Tree a
makeTree base arcs = Tr.unfoldTree getChildren base
  where
    getChildren z = (z, [ y | (x, y) <- arcs, x == z ])

readUtf8File :: FilePath -> IO T.Text
readUtf8File path = TE.decodeUtf8 <$> B.readFile path

parseInputs :: T.Text -> [(T.Text, T.Text)]
parseInputs txt = mkArc <$> T.lines txt
  where
    mkArc = T.breakOn ")" .> second (T.drop 1)
