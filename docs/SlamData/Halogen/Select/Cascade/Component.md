## Module SlamData.Halogen.Select.Cascade.Component

#### `SelectKey`

``` purescript
newtype SelectKey
  = SelectKey String
```

##### Instances
``` purescript
Eq SelectKey
Ord SelectKey
Show SelectKey
```

#### `Option`

``` purescript
newtype Option v
  = Option { key :: SelectKey, value :: v }
```

##### Instances
``` purescript
Eq (Option v)
Ord (Option v)
(Show v) => Show (Option v)
```

#### `State`

``` purescript
type State v = { options :: Set (Option v), selected :: Map Int (Option v), keyMap :: Map SelectKey (Option v) }
```

#### `initialState`

``` purescript
initialState :: forall v. Array v -> (v -> SelectKey) -> State v
```

#### `Query`

``` purescript
data Query v a
  = Selected Int String a
  | Remove Int a
  | GetSelected (Array v -> a)
```

#### `cascadeSelect`

``` purescript
cascadeSelect :: forall v e. CascadeSelectConfig -> Component (State v) (Query v) (Aff e)
```


