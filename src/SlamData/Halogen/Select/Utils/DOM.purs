module SlamData.Halogen.Select.Utils.DOM where

import Prelude

import Control.Monad.Eff (Eff())

import Data.StrMap as Sm

import DOM (DOM())
import DOM.HTML.Types (HTMLElement())

type Screen =
  {
    width :: Int
  , height :: Int
  }

foreign import getScreen
  :: forall e. Eff (dom :: DOM|e) Screen

foreign import getComputedStyle
  :: forall e
   . HTMLElement
  -> Eff (dom :: DOM|e) (Sm.StrMap String)

type ClientRect =
  {
    bottom :: Number
  , top :: Number
  , left :: Number
  , right :: Number
  , height :: Number
  , width :: Number
  }

foreign import getClientRects
  :: forall e
   . HTMLElement
  -> Eff (dom :: DOM|e) (Array ClientRect)
