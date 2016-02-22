## Module SlamData.Halogen.Select.Utils.DOM

#### `Screen`

``` purescript
type Screen = { width :: Int, height :: Int }
```

#### `getScreen`

``` purescript
getScreen :: forall e. Eff (dom :: DOM | e) Screen
```

#### `getComputedStyle`

``` purescript
getComputedStyle :: forall e. HTMLElement -> Eff (dom :: DOM | e) (StrMap String)
```

#### `ClientRect`

``` purescript
type ClientRect = { bottom :: Number, top :: Number, left :: Number, right :: Number, height :: Number, width :: Number }
```

#### `getClientRects`

``` purescript
getClientRects :: forall e. HTMLElement -> Eff (dom :: DOM | e) (Array ClientRect)
```


