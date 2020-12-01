{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.List.NonEmpty qualified as N
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Tree qualified as Tr
import Data.Tuple (swap)
import Flow ((<.), (.>))


main :: IO ()
main = do
    input <- (makeTree <. parseInputs) <$> readUtf8File "input.txt"
    print $ part1 input

part1 :: Eq a => Tr.Tree a -> Int
part1 tree = numDirectOrbits + numIndirectOrbits
  where
    nodes = Tr.flatten tree
    numDirectOrbits = length $ mapMaybe (`directParent` tree) nodes
    numIndirectOrbits = length $ concatMap (`indirectParents` tree) nodes

directChildren :: Eq a => a -> Tr.Tree a -> [a]
directChildren node (Tr.Node x xs)
    | x == node = Tr.rootLabel <$> xs
    | otherwise = concatMap (directChildren node) xs

indirectChildren :: Eq a => a -> Tr.Tree a -> [a]
indirectChildren node (Tr.Node x xs)
    | x == node = concatMap Tr.flatten (concatMap Tr.subForest xs)
    | otherwise = concatMap (indirectChildren node) xs

directParent :: Eq a => a -> Tr.Tree a -> Maybe a
directParent node (Tr.Node x xs)
    | node `elem` fmap Tr.rootLabel xs = Just x
    | otherwise = listToMaybe $ mapMaybe (directParent node) xs

indirectParents :: Eq a => a -> Tr.Tree a -> [a]
indirectParents node tree = drop 1 $ reverse $ go [] node
  where
    go acc x = case directParent x tree of
        Just y -> go (y:acc) y
        Nothing -> acc

makeTree :: Eq a => N.NonEmpty (a, a) -> Tr.Tree a
makeTree arcs = Tr.unfoldTree lookupChildren base
  where
    base = lookupRecurse (swap <$> arcs)
    lookupChildren node = (node, [ y | (x, y) <- N.toList arcs, x == node ])

readUtf8File :: FilePath -> IO T.Text
readUtf8File path = TE.decodeUtf8 <$> B.readFile path

parseInputs :: T.Text -> N.NonEmpty (T.Text, T.Text)
parseInputs txt = mkArc <$> N.fromList (T.lines txt)
  where
    mkArc = T.breakOn ")" .> second (T.drop 1)

lookupRecurse :: Eq a => N.NonEmpty (a, a) -> a
lookupRecurse neList = go $ snd $ N.head neList
  where
    list = N.toList neList
    go x = maybe x go (lookup x list)
