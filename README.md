# Summary

Lawful typeclasses capturing three patterns of bidirectional mapping and forming a layered hierarchy with an ascending strictness of laws.

1. Smart constructor

2. Canonicalization or lossy conversion

3. Isomorphism or lossless conversion

## The conversion problem

Have you ever looked for a `toString` function? How often do you
import `Data.Text.Lazy` only to call its `fromStrict`? How
about importing `Data.ByteString.Builder` only to call its
`toLazyByteString` and then importing
`Data.ByteString.Lazy` only to call its `toStrict`?

Those all are instances of one pattern. They are conversions between different
representations of the same information. Codebases that don't attempt to
abstract over this pattern tend to be sprawling with this type of
boilerplate. It's noise to the codereader, it's a burden to the
implementor and the maintainer.

## Why another conversion library?

Many libraries exist that approach the conversion problem. However most of
them provide lawless typeclasses leaving it up to the author of the
instance to define what makes a proper conversion. This results in
inconsistencies across instances, their behaviour not being evident to
the user and no way to check whether an instance is correct.

This library tackles this problem with a lawful typeclass hierarchy, making it
evident what any of its instances do and it provides property-tests for you
to validate your instances.

# Documentation

- [Haddocks for the latest commit on `master`](https://nikita-volkov.github.io/lawful-conversions)
- [Haddocks for releases on Hackage](https://hackage.haskell.org/package/lawful-conversions)

# Prior work and acknowledgements

This library is an offspring of the "[isomorphism-class](https://hackage.haskell.org/package/isomorphism-class)" library, expanding upon the patterns discovered there. Both libraries are maintained letting their designs compete.

Some ideas and concepts are also shared with the following libraries:

- [control-iso](https://hackage.haskell.org/package/control-iso)
- [type-iso](https://hackage.haskell.org/package/type-iso)
- [injections](https://hackage.haskell.org/package/injections)
