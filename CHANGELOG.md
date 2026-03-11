# v0.4

## Breaking

- Removed interpretation-based relations: UTF-8, ISO-8601, and UUID. 
  - Relations that depend on a specific encoding or format choice violate the library's principle of unambiguous structural conversion — there is no single canonical way to encode Text as bytes or render a UTCTime as a string.

# v0.3

## Breaking

- `from` of `IsMany` renamed to `onfrom`.
- New `from` function added, which is an alias to `to` with type arguments flipped.

# v0.2

## Breaking

- Added more tests in `isManyProperties` and an `Arbitrary` constraint on its parameter `a`.
