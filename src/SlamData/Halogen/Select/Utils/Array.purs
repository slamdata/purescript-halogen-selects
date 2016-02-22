module SlamData.Halogen.Select.Utils.Array where

import Prelude

import Data.Array (length, concat, replicate, snoc, uncons)
import Data.Maybe (Maybe(..))

repeat :: forall a. Int -> Array a -> Array a
repeat n arr = concat $ replicate n arr

shift :: forall a. Int -> Array a -> Array a
shift 0 arr = arr
shift n arr =
  let
    larr = length arr
    dived = n / larr
  in
   if dived < 0
   then shift ((one - dived) * larr + n) arr
   else
     if n > larr
     then shift (n `mod` larr) arr
     else
       case uncons arr of
         Just {head, tail} -> shift (n - one) $ snoc tail head
         Nothing -> [ ]
