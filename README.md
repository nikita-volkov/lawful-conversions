# Lawful Conversions

[![Hackage](https://img.shields.io/hackage/v/lawful-conversions.svg)](https://hackage.haskell.org/package/lawful-conversions)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/lawful-conversions/)

A Haskell library providing **lawful typeclasses for bidirectional type conversions**, grounded in mathematical principles from set theory and category theory.

## Core Concept

This library defines a precise hierarchy of three conversion patterns with increasing mathematical strictness:

1. **Smart Constructor** (`IsSome`) - Subset embedding with partial inverse
2. **Canonicalization** (`IsMany`) - Many-to-one lossy conversion  
3. **Isomorphism** (`Is`) - Bidirectional lossless conversion

Each typeclass comes with **mathematical laws** and **property-based tests** to ensure correctness and consistency across all instances.

## The Problem: Conversion Boilerplate

Have you ever found yourself writing code like this?

```haskell
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS

-- Converting between different representations
result = LBS.toStrict $ BB.toLazyByteString $ 
         BB.byteString $ TE.encodeUtf8 $ LT.toStrict input
```

These are all instances of the same fundamental pattern: **converting between different representations of the same information**. Without proper abstraction, codebases become littered with this boilerplate, creating noise for readers and maintenance burden for developers.

## Why Lawful Conversions?

Many conversion libraries exist, but most provide **lawless typeclasses** that leave correctness up to instance authors. This leads to:

- **Inconsistent behavior** across instances
- **Unpredictable semantics** for library users  
- **No verification** that instances are correct

**Lawful Conversions** solves these problems with:

- ✅ **Mathematical rigor** - Laws based on set theory and category theory
- ✅ **Property-based testing** - Automated verification of law compliance
- ✅ **Clear semantics** - Predictable behavior across all instances
- ✅ **Type safety** - Compile-time guarantees about conversion relationships

## Quick Start

```haskell
{-# LANGUAGE TypeApplications #-}
import LawfulConversions

-- Merge various inputs into a bytestring using builder.
combineEncodings ::
  [Word8] ->
  Data.ByteString.Lazy.ByteString ->
  Data.ByteString.Short.ShortByteString ->
  Data.Primitive.ByteArray ->
  Data.ByteString.ByteString
combineEncodings a b c d =
  from @Data.ByteString.Builder.Builder $
    to a <> to b <> to c <> to d

-- Partial conversion with failure handling
maybePercent :: Maybe Percent
maybePercent = maybeFrom @Double 0.75  -- Just (Percent 0.75)

badPercent :: Maybe Percent  
badPercent = maybeFrom @Double 1.5     -- Nothing (out of range)
```

## Typeclass Hierarchy

### `IsSome a b` - Smart Constructor Pattern
Evidence that type `b` is a **subset** of type `a`:
- `to :: b -> a` (total injection)
- `maybeFrom :: a -> Maybe b` (partial inverse)
- **Law**: Injectivity and partial inverse relationship

### `IsMany a b` - Canonicalization Pattern  
Evidence that type `a` can be **canonicalized** to type `b`:
- `onfrom :: a -> b` (total surjection)
- **Law**: Consistent with subset relationships

### `Is a b` - Isomorphism Pattern
Evidence that types `a` and `b` are **isomorphic**:
- Combines both `IsSome` and `IsMany`
- **Law**: `to` and `from` are total inverses

## Supported Type Conversions

The library includes instances for common Haskell types:

- **Text types**: `String` ↔ `Text` ↔ `LazyText` ↔ `Builder`
- **ByteString types**: `ByteString` ↔ `LazyByteString` ↔ `ShortByteString` ↔ `ByteArray`
- **Numeric types**: `Int8` ↔ `Word8`, `Int16` ↔ `Word16`, etc.
- **Collections**: `Vector` ↔ `List` ↔ `Seq`, `IntMap` ↔ `Map Int`

All instances are **mathematically verified** through property-based testing.

## Documentation

- 📚 [**API Documentation**](https://hackage.haskell.org/package/lawful-conversions) - Complete Haddock documentation on Hackage
- 🔧 [**Development Docs**](https://nikita-volkov.github.io/lawful-conversions/) - Latest documentation from `master` branch

## Related Libraries & Prior Work

This library builds upon and competes with several existing conversion libraries:

- **[isomorphism-class](https://hackage.haskell.org/package/isomorphism-class)** - The predecessor library that inspired this work
- **[control-iso](https://hackage.haskell.org/package/control-iso)** - Control structure isomorphisms  
- **[type-iso](https://hackage.haskell.org/package/type-iso)** - Type-level isomorphisms
- **[injections](https://hackage.haskell.org/package/injections)** - Injection-focused conversions

**Key differentiator**: Lawful Conversions provides a complete mathematical foundation with property-based verification, ensuring correctness and predictability across all instances.

## Contributing

This library prioritizes **mathematical correctness** and **comprehensive testing**. When contributing:

1. Ensure all instances satisfy mathematical laws
2. Add property-based tests for new conversions  
3. Follow the established module structure under `Relations/`
4. Maintain compatibility across supported versions of GHC and libraries
