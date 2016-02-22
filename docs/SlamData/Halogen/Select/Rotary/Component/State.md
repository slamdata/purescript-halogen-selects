## Module SlamData.Halogen.Select.Rotary.Component.State

#### `VisualState`

``` purescript
data VisualState
  = Staying
  | Dragging Number
  | Animating Number Number
```

#### `Option`

``` purescript
newtype Option r
  = Option { label :: String | r }
```

#### `runOption`

``` purescript
runOption :: forall r. Option r -> { label :: String | r }
```

#### `State`

``` purescript
type State r = { visualState :: VisualState, styles :: CSS, element :: Maybe HTMLElement, position :: Number, key :: Maybe String, items :: Circular Array (Option r), displayedItems :: Array (Option r), constStyles :: CSS }
```

#### `_visualState`

``` purescript
_visualState :: forall a r. LensP { visualState :: a | r } a
```

#### `_styles`

``` purescript
_styles :: forall a r. LensP { styles :: a | r } a
```

#### `_element`

``` purescript
_element :: forall a r. LensP { element :: a | r } a
```

#### `_position`

``` purescript
_position :: forall a r. LensP { position :: a | r } a
```

#### `_key`

``` purescript
_key :: forall a r. LensP { key :: a | r } a
```

#### `_items`

``` purescript
_items :: forall a r. LensP { items :: a | r } a
```

#### `_displayedItems`

``` purescript
_displayedItems :: forall a r. LensP { displayedItems :: a | r } a
```

#### `_constStyles`

``` purescript
_constStyles :: forall a r. LensP { constStyles :: a | r } a
```

#### `isDragged`

``` purescript
isDragged :: forall r. State r -> Boolean
```

#### `updateStyles`

``` purescript
updateStyles :: forall r. State r -> State r
```

#### `initialState`

``` purescript
initialState :: forall r. NonEmpty Array (Option r) -> State r
```


