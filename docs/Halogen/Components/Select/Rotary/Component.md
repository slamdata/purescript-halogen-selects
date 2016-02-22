## Module Halogen.Components.Select.Rotary.Component

#### `comp`

``` purescript
comp :: forall r e. RotarySelectorConfig r -> { component :: Component State Query (AffSel e), unpack :: OptionR -> Option r }
```

#### `rotarySelect`

``` purescript
rotarySelect :: forall r p e. RotarySelectorConfig r -> Array (Option r) -> p -> { slot :: SlotConstructor State Query (AffSel e) p, unpack :: OptionR -> Option r }
```


