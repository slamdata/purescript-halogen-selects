module Data.Circular where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.MonadPlus (guard)

import Data.NonEmpty as Ne
import Data.Array as A
import Data.Foldable as F
import Data.Maybe as M
import Data.Unfoldable as Uf
import Data.Tuple as Tp
import Data.Functor (($>))

-- uncons empty = Nothing
-- uncons (pure x <|> tail) = Just { head: x, tail: tail }
class (Alternative f) <= Uncons f where
  uncons :: forall a. f a -> M.Maybe {head :: a, tail :: f a}

instance arrayUncons :: Uncons Array where
  uncons = A.uncons

newtype Circular f a = Circular (Ne.NonEmpty f a)
runCircular :: forall f a. Circular f a -> Ne.NonEmpty f a
runCircular (Circular fa) = fa

instance functorCircular :: (Functor f) => Functor (Circular f) where
  map f (Circular nefa) = Circular $ map f nefa

lengthCircular
  :: forall f a
   . (F.Foldable f)
  => Circular f a
  -> Int
lengthCircular (Circular nefa) =
  F.foldl (\b _ -> b + one) zero nefa

pointed :: forall f a. Circular f a -> a
pointed (Circular nefa) = Ne.head nefa

shift
  :: forall f a
   . (Uncons f, F.Foldable f)
  => Int
  -> Circular f a
  -> Circular f a
shift n circ =
  shift' n circ
  where
  nefa :: Ne.NonEmpty f a
  nefa = runCircular circ

  len :: Int
  len = lengthCircular circ

  dived :: Int
  dived = n / len

  shift'
    :: Int -> Circular f a -> Circular f a
  shift' 0 circ =
    circ
  shift' n circ
    | dived < 0 = shift ((one - dived) * len + n) circ
  shift' n circ
    | n > len = shift (n `mod` len) circ
  shift' n circ =
    case uncons $ Ne.tail nefa of
      M.Just {head = hd, tail = tl} ->
        shift (n - one)
        $ Circular
        $ hd Ne.:| (tl <|> (pure $ Ne.head nefa))
      M.Nothing ->
        circ

takeLeft
  :: forall f a u
   . (F.Foldable f, Uncons f, Uf.Unfoldable u)
  => Int
  -> Circular f a
  -> u a
takeLeft n circ = Uf.unfoldr step (Tp.Tuple n circ)
  where
  step
    :: Tp.Tuple Int (Circular f a)
    -> M.Maybe (Tp.Tuple a (Tp.Tuple Int (Circular f a)))
  step (Tp.Tuple n circ) = do
    guard (n > 0) $> Tp.Tuple (pointed circ) (Tp.Tuple (n - one) (shift 1 circ))


samples
  :: forall f a u
   . (F.Foldable f, Uncons f, Uf.Unfoldable u)
  => Int
  -> Circular f a
  -> u a
samples n circ = takeLeft (n * lengthCircular circ) circ
