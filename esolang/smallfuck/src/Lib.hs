module Lib
  ( interpreter
  ) where

import Data.Maybe (fromJust)
import Data.Vector ((//), (!))

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

commands :: String
commands = "<>*[]"

-- Only takes sane (filtered) inputs.
makeJumpTable :: String -> Maybe (M.Map Int Int)
makeJumpTable cmds = go cmds 0 [] M.empty
  where
    go cmds i opStack accMap =
      case (cmds, opStack) of
        ([], []) -> Just accMap
        ([], _) -> Nothing -- Invalid commands with remaining brackets.
        ('[':cmdTl, st) -> go cmdTl (i + 1) (i : st) accMap
        (']':cmdTl, p:stTl) ->
          go cmdTl (i + 1) stTl $ M.insert i p (M.insert p i accMap)
        (']':_, _) -> Nothing -- No matching bracket.
        (_:cmdTl, st) -> go cmdTl (i + 1) st accMap

-- Input data is guaranteed to be sane.
interpreter :: String -> String -> String
interpreter rawCode inputData = go 0 0 $ V.fromList inputData
  where
    code = filter (\ch -> elem ch commands) rawCode
    cmds = V.fromList code
    cmdLen = V.length cmds
    dataLen = length inputData
    jumpTable = fromJust $ makeJumpTable code
    flip i v = v // [(i, new)]
      where
        new =
          if (v ! i == '0')
            then '1'
            else '0'
    go cmdP dataP acc =
      if (dataP < 0 || dataP >= dataLen || cmdP >= cmdLen)
        then V.toList acc
        else case cmds ! cmdP of
               '*' -> go nCmdP dataP $ flip dataP acc
               '>' -> go nCmdP (dataP + 1) acc
               '<' -> go nCmdP (dataP - 1) acc
               '[' ->
                 if (acc ! dataP == '0')
                   then go ((fromJust $ M.lookup cmdP jumpTable) + 1) dataP acc
                   else go nCmdP dataP acc
               ']' ->
                 if (acc ! dataP /= '0')
                   then go (fromJust $ M.lookup cmdP jumpTable) dataP acc
                   else go nCmdP dataP acc
      where
        nCmdP = cmdP + 1
