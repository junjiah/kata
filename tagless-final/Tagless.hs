{-# LANGUAGE RankNTypes #-}

module Tagless where

import           Prelude hiding (and, or)


class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> (r h a -> r h b)

  loop   :: r h (a -> a) -> r h a

  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int    -- \x -> x - 1
  up     :: r h Int -> r h Int    -- \x -> x + 1
  mult   :: r h Int -> r h Int -> r h Int
  gte    :: r h Int -> r h Int -> r h Bool

  bool   :: Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool

  ifte   :: r h Bool -> r h a -> r h a -> r h a


-- The interpreter.
-- http://okmij.org/ftp/tagless-final/course/lecture.pdf
newtype R h a = R { unR :: h -> a}

instance Language R where
  here = R $ \(x, _) -> x
  before v = R $ \(_, h) -> unR v h
  lambda e = R $ \h -> \x -> unR e (x, h)
  apply e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)

  loop e = R $ \h -> fix (unR e h) where fix f = f $ fix f

  int x = R $ const x
  add e1 e2 = R $ \h -> (unR e1 h) + (unR e2 h)
  down e = R $ \h -> unR e h - 1
  up e = R $ \h -> unR e h + 1
  mult e1 e2 = R $ \h -> (unR e1 h) * (unR e2 h)
  gte e1 e2 = R $ \h -> (unR e1 h) >= (unR e2 h)

  bool x = R $ const x
  and e1 e2 = R $ \h -> (unR e1 h) && (unR e2 h)
  or e1 e2 = R $ \h -> (unR e1 h) || (unR e2 h)
  neg e = R $ \h -> not $ (unR e h)

  ifte c t e = R $ \h -> if (unR c h) then (unR t h) else (unR e h)

type Term a = forall r h . Language r => r h a

interpret :: Term a -> a
interpret t = unR t ()
