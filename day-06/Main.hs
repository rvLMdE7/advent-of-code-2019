{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (second)
import Data.ByteString qualified as B
import Data.List qualified as L
import Data.List.NonEmpty qualified as N
import Data.Maybe (fromJust, mapMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Tree qualified as Tr
import Data.Tuple (swap)
import Flow ((<.), (.>))


main :: IO ()
main = do
    input <- (makeTree <. parseInputs) <$> readUtf8File "input.txt"
    print $ part1 input
    print $ part2 input

part1 :: Tr.Tree T.Text -> Int
part1 tree = length $ concatMap (`allParents` tree) (Tr.flatten tree)

part2 :: Tr.Tree T.Text -> Int
part2 tree = fromJust $ distance "YOU" "SAN" tree

directParent :: Eq a => a -> Tr.Tree a -> Maybe a
directParent node (Tr.Node x xs)
    | node `elem` fmap Tr.rootLabel xs = Just x
    | otherwise = listToMaybe $ mapMaybe (directParent node) xs

allParents :: Eq a => a -> Tr.Tree a -> [a]
allParents node tree = reverse $ go [] node
  where
    go acc x = case directParent x tree of
        Just y -> go (y:acc) y
        Nothing -> acc

distance :: Eq a => a -> a -> Tr.Tree a -> Maybe Int
distance x y tree = do
    let xParents = allParents x tree
        yParents = allParents y tree
    commonAncestor <- listToMaybe (xParents `L.intersect` yParents)
    xDist <- L.elemIndex commonAncestor xParents
    yDist <- L.elemIndex commonAncestor yParents
    pure $ xDist + yDist

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
