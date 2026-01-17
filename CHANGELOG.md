# v0.4

## Breaking

- **Merged typeclasses**: `IsSome` and `IsMany` have been merged into a single `IsSubsetOf` typeclass
  - The `IsSubsetOf` typeclass now provides both `maybeFrom` (partial inverse) and `onfrom` (lossy canonicalization)
  - This simplifies the API while preserving all functionality
- **Renamed typeclass**: `IsSome` → `IsSubsetOf`
- **Removed typeclass**: `IsMany` (functionality merged into `IsSubsetOf`)
- **Renamed functions**:
  - `isSomeProperties` → `isSubsetOfProperties`
  - `isManyProperties` removed (tests merged into `isSubsetOfProperties`)
  - `isSomePrism` → `isSubsetOfPrism`
  - `isManyIso` → `isSubsetOfIso`
- **Renamed module**: `LawfulConversions.Proxies.ViaIsSome` → `LawfulConversions.Proxies.ViaIsSubsetOf`
- **Removed instances**: Removed time and UUID related conversion instances:
  - `Day` conversions (to/from `String`, `Text`, `LazyText`, and text builders)
  - `UtcTime` conversions (to/from `String`, `Text`, `LazyText`, and text builders)
  - `Uuid` conversions (to/from `String`, `Text`, `LazyText`, and text builders)
- **Removed dependencies**: `time`, `uuid-types`, and `unordered-containers` are no longer dependencies of this library.

## Migration Guide

To update your code:

1. Replace all occurrences of `IsSome` with `IsSubsetOf`
2. Remove separate `IsMany` instances - merge the `onfrom` implementation into your `IsSubsetOf` instance
3. Update test property names: `isSomeProperties` → `isSubsetOfProperties`
4. Update optics function names: `isSomePrism` → `isSubsetOfPrism`, `isManyIso` → `isSubsetOfIso`
5. If using `ViaIsSome` proxy, rename to `ViaIsSubsetOf`

# v0.3

## Breaking

- `from` of `IsMany` renamed to `onfrom`.
- New `from` function added, which is an alias to `to` with type arguments flipped.

# v0.2

## Breaking

- Added more tests in `isManyProperties` and an `Arbitrary` constraint on its parameter `a`.
