## Module Halogen.Components.Select.Utils.NonEmpty

#### `Uncons`

``` purescript
class (Alternative f) <= Uncons f where
  uncons :: forall a. f a -> Maybe { head :: a, tail :: f a }
```

##### Instances
``` purescript
Uncons Array
```

#### `liftNonEmpty`

``` purescript
liftNonEmpty :: forall f a b. (Uncons f) => (f a -> f b) -> NonEmpty f a -> NonEmpty f b
```


