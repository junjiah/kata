module Rule30 where

import           Data.Vector ((!?))
import qualified Data.Vector as V

toValidVector :: [Int] -> V.Vector Int
toValidVector = V.fromList . map (\x -> if x /= 1 then 0 else 1)

rule :: Int -> Int -> Int -> Int
rule 0 0 0 = 0
rule 0 0 1 = 1
rule 0 1 0 = 1
rule 0 1 1 = 1
rule 1 0 0 = 1
rule 1 0 1 = 0
rule 1 1 0 = 0
rule 1 1 1 = 0
rule _ _ _ = error "invalid input"

rule30 :: [Int] -> Int -> [Int]
rule30 cells n
  | n <= 0 = cells
  | otherwise = go (toValidVector cells) n
  where go vs 0 = V.toList vs
        go vs n = go (applyRule vs) (n-1)

applyRule :: V.Vector Int -> V.Vector Int
applyRule v = V.imap (\i val -> rule (get (i-1)) val (get (i+1))) filled
  where filled = V.snoc (V.cons 0 v) 0   -- Fill 0 at the start and end.
        get i = maybe 0 id (filled !? i) -- Getter func.
