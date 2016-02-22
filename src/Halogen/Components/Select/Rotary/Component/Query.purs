module Halogen.Components.Select.Rotary.Component.Query where

import DOM.HTML.Types as Ht
import Halogen.Components.Select.Rotary.Component.State (OptionR())

data Query a
  = Init Ht.HTMLElement a
  | StartDragging Number a
  | StopDragging a
  | Animated a
  | ChangePosition Number a
  | GetSelected (OptionR -> a)
  | Selected OptionR a
