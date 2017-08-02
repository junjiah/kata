module FunctionEvaluator where

import qualified Data.Map.Strict as M

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f x = snd $ memoize x M.empty
  where
    memoize n m = case M.lookup n m of
      Just v -> (m, v)
      Nothing ->
        case f n of
          Left v -> (M.insert n v m, v)
          Right (as, aggF) ->
            let folder = \(accM, accBs) a -> let (m, v) = memoize a accM in (m, v:accBs) in
            let (m', bs) = foldl folder (m, []) as in
            let v = aggF bs in (M.insert n v m', v)