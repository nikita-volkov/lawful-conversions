Lawful typeclasses capturing three main patterns of bidirectional mapping. The typeclasses form a layered hierarchy with ascending strictness of laws:

1. `IsSome`: Smart constructor pattern

2. `IsMany`: Lossy conversion

3. `Is`: Isomorphism

The library is an offspring from the ["isomorphism-class"](https://hackage.haskell.org/package/isomorphism-class) library.