module SlamData.Halogen.Select.Utils.Random where

import Prelude

import Control.Monad.Aff.Free (class Affable, fromEff)
import Control.Monad.Eff.Random (random, RANDOM)

import Data.Date (nowEpochMilliseconds, Now)
import Data.String.Regex as Rgx
import Data.Time (Milliseconds(..))

randomString
  :: forall e g
   . (Affable (random :: RANDOM|e) g)
  => g String
randomString =
  fromEff
    $ random
    <#> mkString

foreign import toString
  :: Int
  -> Number
  -> String

mkString :: Number -> String
mkString = toString 36 >>> Rgx.replace numAndDot ""
  where
  numAndDot = Rgx.regex "(\\d|\\.)" Rgx.noFlags{global=true}

-- | Generate unique key for component
genKey
  :: forall eff g
   . (Monad g, Affable (now :: Now, random :: RANDOM|eff) g)
  => g String
genKey = do
  rn1 <- randomString
  rn2 <- randomString
  (Milliseconds time) <- fromEff nowEpochMilliseconds
  pure $ rn1 <> mkString time <> rn2
