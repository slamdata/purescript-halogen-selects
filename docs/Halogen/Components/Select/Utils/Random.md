## Module Halogen.Components.Select.Utils.Random

#### `randomString`

``` purescript
randomString :: forall e g. (FunctorEff (random :: RANDOM | e) g) => g String
```

#### `toString`

``` purescript
toString :: forall e. Int -> Number -> String
```

#### `mkString`

``` purescript
mkString :: Number -> String
```

#### `genKey`

``` purescript
genKey :: forall eff g. (FunctorEff (now :: Now, random :: RANDOM | eff) g) => g String
```

Generate unique key for component


