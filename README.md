# Summary

Lawful typeclasses capturing three main patterns of bidirectional mapping. The typeclasses form a layered hierarchy with ascending strictness of laws:

1. `IsSome`: Smart constructor pattern

2. `IsMany`: Lossy conversion

3. `Is`: Isomorphism (lossless conversion)

# The conversion problem

Have you ever looked for a `toString` function? How often do you
import `Data.Text.Lazy` only to call its `Data.Text.Lazy.fromStrict`? How
about importing `Data.ByteString.Builder` only to call its
`Data.ByteString.Builder.toLazyByteString` and then importing
`Data.ByteString.Lazy` only to call its `Data.ByteString.Lazy.toStrict`?

Those all are instances of one pattern. They are conversions between
representations of the same information. Codebases that don't attempt to
abstract over this pattern tend to be sprawling with this type of
boilerplate. It's noise to the codereader, it's a burden to the
implementor and the maintainer.

# Why another conversion library?

Many libraries exist that approach the conversion problem. However most of
them provide lawless typeclasses leaving it up to the author of the
instance to define what makes a proper conversion. This results in
inconsistencies across instances, their behaviour not being evident to
the user and no way to check whether an instance is correct.

This library tackles this problem with a lawful typeclass hierarchy, making it
evident what any of its instances do and it provides property-tests for you
to validate your instances.