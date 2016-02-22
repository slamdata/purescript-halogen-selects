module SlamData.Halogen.Select.Utils.NonEmpty where

import Prelude

import Control.Alternative (Alternative)
import Data.NonEmpty (NonEmpty(), (:|), oneOf)
import Data.Maybe as M
import Data.Maybe.Unsafe as Mu
import Data.Array as A

-- uncons empty = Nothing
-- uncons (pure x <|> tail) = Just { head: x, tail: tail }
class (Alternative f) <= Uncons f where
  uncons :: forall a. f a -> M.Maybe {head :: a, tail :: f a}

instance arrayUncons :: Uncons Array where
  uncons = A.uncons

liftNonEmpty
  :: forall f a b
   . (Uncons f)
  => (f a -> f b)
  -> NonEmpty f a
  -> NonEmpty f b
liftNonEmpty fab nefa =
  let
    fa :: f a
    fa = oneOf nefa

    fb :: f b
    fb = fab fa

    mbUnconsed :: M.Maybe {head :: b, tail :: f b}
    mbUnconsed = uncons fb

    -- Safe, `isJust (uncons (pure x <|> tail)) <-- oneOf === (pure x <|> tail)`
    unconsed :: {head :: b, tail :: f b}
    unconsed = Mu.fromJust mbUnconsed
  in
   unconsed.head :| unconsed.tail
