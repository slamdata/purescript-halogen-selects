module SlamData.Halogen.Select.Rotary.Component.Query where

import Data.Maybe (Maybe())
import DOM.HTML.Types as Ht
import SlamData.Halogen.Select.Rotary.Component.State (Option())

data Query r a
  = SetElement (Maybe Ht.HTMLElement) a
  | Init a
  | StartDragging Number a
  | StopDragging a
  | Animated a
  | ChangePosition Number a
  | GetSelected (Option r -> a)
  | Selected (Option r) a
