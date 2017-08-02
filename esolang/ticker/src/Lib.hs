module Lib
  ( interpreter
  ) where

import Data.Char (chr)
import Data.Vector ((//), (!?))

import qualified Data.Vector as V

interpreter :: String -> String
interpreter cmds = go cmds (V.singleton 0) 0 ""
  where
    outputCell mem i =
      case mem !? i of
        Nothing -> 0
        Just v -> v
    incr v = mod (v + 1) 256
    decr v = mod (v - 1) 256
    update mem i updateFunc =
      case mem !? i of
        Nothing -> mem
        Just v -> mem // [(i, updateFunc v)]
    go cmds mem memP output =
      case cmds of
        '>':cmdTl -> go cmdTl mem (memP + 1) output
        '<':cmdTl -> go cmdTl mem (memP - 1) output
        '*':cmdTl -> go cmdTl mem memP $ (chr $ outputCell mem memP) : output
        '+':cmdTl -> go cmdTl (update mem memP incr) memP output
        '-':cmdTl -> go cmdTl (update mem memP decr) memP output
        '/':cmdTl -> go cmdTl (update mem memP (\_ -> 0)) memP output
        '!':cmdTl -> go cmdTl (V.snoc mem 0) memP output
        _:cmdTl -> go cmdTl mem memP output
