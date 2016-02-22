## Module Data.Circular

#### `Uncons`

``` purescript
class (Alternative f) <= Uncons f where
  uncons :: forall a. f a -> Maybe { head :: a, tail :: f a }
```

##### Instances
``` purescript
Uncons Array
```

#### `Circular`

``` purescript
newtype Circular f a
  = Circular (NonEmpty f a)
```

##### Instances
``` purescript
(Functor f) => Functor (Circular f)
```

#### `runCircular`

``` purescript
runCircular :: forall f a. Circular f a -> NonEmpty f a
```

#### `lengthCircular`

``` purescript
lengthCircular :: forall f a. (Foldable f) => Circular f a -> Int
```

#### `pointed`

``` purescript
pointed :: forall f a. Circular f a -> a
```

#### `shift`

``` purescript
shift :: forall f a. (Uncons f, Foldable f) => Int -> Circular f a -> Circular f a
```

#### `takeLeft`

``` purescript
takeLeft :: forall f a u. (Foldable f, Uncons f, Unfoldable u) => Int -> Circular f a -> u a
```

#### `samples`

``` purescript
samples :: forall f a u. (Foldable f, Uncons f, Unfoldable u) => Int -> Circular f a -> u a
```


