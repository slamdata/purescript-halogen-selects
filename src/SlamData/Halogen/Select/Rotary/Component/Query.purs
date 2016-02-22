module SlamData.Halogen.Select.Rotary.Component.Query where

import DOM.HTML.Types as Ht
import SlamData.Halogen.Select.Rotary.Component.State (Option())

data Query r a
  = Init Ht.HTMLElement a
  | StartDragging Number a
  | StopDragging a
  | Animated a
  | ChangePosition Number a
  | GetSelected (Option r -> a)
  | Selected (Option r) a
