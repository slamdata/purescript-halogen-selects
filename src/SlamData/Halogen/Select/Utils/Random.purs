module SlamData.Halogen.Select.Utils.Random where

import Prelude

import Control.Monad.Eff.Random (random, RANDOM())

import Data.Date (nowEpochMilliseconds, Now())
import Data.Functor.Eff (FunctorEff, liftEff)
import Data.String.Regex as Rgx
import Data.Time (Milliseconds(..))

randomString
  :: forall e g
   . (FunctorEff (random :: RANDOM|e) g)
  => g String
randomString =
  liftEff
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
   . (FunctorEff (now :: Now, random :: RANDOM|eff) g)
  => g String
genKey = liftEff do
  rn1 <- randomString
  rn2 <- randomString
  (Milliseconds time) <- nowEpochMilliseconds
  pure $ rn1 <> mkString time <> rn2
