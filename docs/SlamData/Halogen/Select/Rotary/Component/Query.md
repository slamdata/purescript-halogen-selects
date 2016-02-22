## Module SlamData.Halogen.Select.Rotary.Component.Query

#### `Query`

``` purescript
data Query r a
  = Init HTMLElement a
  | StartDragging Number a
  | StopDragging a
  | Animated a
  | ChangePosition Number a
  | GetSelected (Option r -> a)
  | Selected (Option r) a
```


