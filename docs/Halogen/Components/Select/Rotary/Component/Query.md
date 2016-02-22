## Module Halogen.Components.Select.Rotary.Component.Query

#### `Query`

``` purescript
data Query a
  = Init HTMLElement a
  | StartDragging Number a
  | StopDragging a
  | Animated a
  | ChangePosition Number a
  | GetSelected (OptionR -> a)
  | Selected OptionR a
```


