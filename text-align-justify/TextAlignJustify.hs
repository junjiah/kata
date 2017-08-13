module TextAlignJustify where

import           Data.List           (intercalate)

import qualified Control.Monad.State as S

-- | Justify a list words with given width.
format :: [String] -> Int -> String
format [] _ = error "should be unreachable"
format [word] _ = word
format wordList width = concat $ zipWith (++) wordList spaces
  where
    -- Some simple math to calculate spaces.
    n = width - (sum $ map length wordList)
    w = length wordList - 1
    avgSpace = quot n w
    -- Number of different space strings.
    b = n - w * avgSpace
    spacesB = replicate b $ replicate (avgSpace + 1) ' '
    a = w - b
    spacesA = replicate a $ replicate avgSpace ' '
    spaces = spacesB ++ spacesA ++ repeat ""

-- | Justify a signle-line text.
justify :: String -> Int -> String
justify text width = fst $ S.runState (go (words text) [] 0) []
  where
    -- Use state monad for learning purposes.
    go :: [String] -> [String] -> Int -> S.State [String] String
    go [] [] _ = do
      res <- S.get
      return $ intercalate "\n" $ reverse res
    go xs acc len
      | null xs || length (head xs) + len > width = do
          res <- S.get
          let acc' = reverse acc
          let line = if null xs then unwords acc' else format acc' width
          S.put $ line:res
          go xs [] 0
      | otherwise = let w = head xs in go (tail xs) (w:acc) (len + length w + 1)
