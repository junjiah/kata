module Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import           Data.List       (sortOn)
import           Data.Maybe      (fromJust)

import qualified Data.Map.Strict as M

data Bit = Z | O deriving (Eq, Show)

data HuffmanTree a =
    Leaf a Int | Node (HuffmanTree a) Int (HuffmanTree a) deriving Show

-- | Get value of a Huffman tree node.
treeVal :: HuffmanTree a -> Int
treeVal (Leaf _ v)   = v
treeVal (Node _ v _) = v

-- | Merge Huffman tree nodes.
mergeTree :: HuffmanTree a -> HuffmanTree a -> HuffmanTree a
mergeTree tr1 tr2 = Node tr1 (treeVal tr1 + treeVal tr2) tr2

-- | Construct a Huffman tree from ascending frequency list.
constructHuffmanTree :: Ord a => [(a, Int)] -> Maybe (HuffmanTree a)
constructHuffmanTree = merge . map (uncurry Leaf) . sortOn snd
    where
        merge []           = Nothing
        merge [Leaf _ _]   = Nothing
        merge [tr]         = Just tr
        merge (hd1:hd2:tl) = merge $ sortOn treeVal $ (mergeTree hd1 hd2):tl

-- | Get the mapping from the Huffman tree element to its bit path.
getEncodingMap :: Ord a => HuffmanTree a -> M.Map a [Bit]
getEncodingMap = go [] M.empty
    where
        go path m (Leaf a _)          = M.insert a (reverse path) m
        go path m (Node left _ right) = go (O:path) (go (Z:path) m left) right

-- | Follow the Huffman tree with given path, return leaf element and the remaining path.
follow :: Ord a => HuffmanTree a -> [Bit] -> (Maybe a, [Bit])
follow (Leaf a _) bits         = (Just a, bits)
follow (Node left _ _) (Z:tl)  = follow left tl
follow (Node _ _ right) (O:tl) = follow right tl
follow _ []                    = (Nothing, [])

-- | Calculate symbol frequencies of a text. Result list is sorted.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = M.toAscList . foldl f M.empty
    where
        f = \acc a -> M.insert a (succ $ M.findWithDefault 0 a acc) acc

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode freqList vals = do
    tr <- constructHuffmanTree freqList
    let m = getEncodingMap tr
    return $ vals >>= \v -> fromJust $ M.lookup v m

-- | Decode a bit sequence using the given frequencies.
decode :: Ord a => [(a, Int)] -> [Bit] -> Maybe [a]
decode freqList bits = do
    tr <- constructHuffmanTree freqList
    go tr bits []
    where
        go _ [] acc = Just $ reverse acc
        go tr path acc = case (follow tr path) of
            (Nothing, _)    -> Nothing
            (Just a, path') -> go tr path' (a:acc)
