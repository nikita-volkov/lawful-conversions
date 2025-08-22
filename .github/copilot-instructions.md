# Copilot Instructions for lawful-conversions

## Repository Overview

This is a Haskell library that provides **lawful typeclasses for bidirectional conversion between types**. The library defines a hierarchy of three mathematical patterns for type conversions:

1. **Smart constructor** (`IsSome`) - One-way conversion with partial inverse
2. **Canonicalization/lossy conversion** (`IsMany`) - Many-to-one total conversion  
3. **Isomorphism/lossless conversion** (`Is`) - Bidirectional lossless conversion

The library is grounded in **set theory and category theory**, providing mathematical laws and property-based tests to ensure correctness.

## Core Concepts

### Typeclass Hierarchy

- `IsSome a b`: Evidence that type `b` is a subset of type `a`
  - `to :: b -> a` (total injection)
  - `maybeFrom :: a -> Maybe b` (partial inverse)
  - **Law**: `to` is injective, `maybeFrom` is partial inverse of `to`

- `IsMany a b`: Evidence that type `a` can be canonicalized to type `b`  
  - `from :: a -> b` (total surjection)
  - **Law**: `from` is consistent with subset relationship

- `Is a b`: Evidence that types `a` and `b` are isomorphic
  - Combines both `IsSome` and `IsMany` with inverse laws
  - **Law**: `to` and `from` are total inverses

### Key Functions

- `to @TargetType value` - Convert from subset to superset type
- `from @SourceType value` - Convert from superset to subset type  
- `maybeFrom value` - Partial inverse conversion

## Project Structure

```
src/
├── library/
│   ├── LawfulConversions.hs              # Main module
│   ├── LawfulConversions/
│   │   ├── Algebra.hs                    # Core typeclass definitions
│   │   ├── Properties.hs                 # QuickCheck property tests
│   │   ├── Relations/                    # Type conversion instances
│   │   │   ├── ByteStringAndText.hs      # ByteString ↔ Text conversions
│   │   │   ├── LazyTextAndStrictText.hs  # Lazy ↔ Strict Text
│   │   │   └── ...                       # Many other type relations
│   │   ├── Proxies/                      # DerivingVia helpers
│   │   └── Optics.hs                     # Lens/Prism integration
│   └── TextCompat/                       # Internal compatibility layer
└── test/
    ├── Main.hs                           # Test suite entry point
    └── Test/ExtraInstances.hs            # Additional test instances
```

## Build System & Commands

This project uses **Cabal** as the build system:

```bash
# Update package index
cabal update

# Build the library
cabal build

# Run tests (comprehensive property-based testing)
cabal test

# Build with specific GHC version
cabal build --with-ghc=ghc-9.2.4

# Generate documentation  
cabal haddock
```

### Dependencies

- **Core**: `base`, `text`, `bytestring`, `containers`, `vector`, `primitive`
- **Testing**: `QuickCheck`, `tasty`, `tasty-quickcheck`, `quickcheck-instances`
- **Mathematical**: Property-based testing validates mathematical laws

## Development Guidelines

### Code Style

- **Language**: Haskell 2010 with common extensions (`TypeApplications`, `ScopedTypeVariables`, etc.)
- **Prelude**: Uses custom `LawfulConversions.Prelude` (no implicit Prelude)
- **Naming**: Module names follow `LawfulConversions.Category.Specific` pattern
- **Documentation**: Extensive Haddock documentation with mathematical foundations
- **Instances**: Each type relation lives in its own module under `Relations/`

### Creating New Instances

When adding conversions between types `A` and `B`:

1. Create `src/library/LawfulConversions/Relations/AAndB.hs`
2. Import in `src/library/LawfulConversions/Relations.hs`  
3. Add property tests in `src/test/Main.hs`
4. Ensure mathematical laws hold:
   - Injectivity for `IsSome`
   - Partial inverse properties
   - Isomorphism laws for `Is`

### Testing Philosophy

- **Property-based testing** using QuickCheck
- Tests validate **mathematical laws**, not just functionality
- Each conversion pair has comprehensive law verification
- Tests run with high iteration counts (10,000+ in CI)

### Mathematical Rigor

This library prioritizes **mathematical correctness**:
- Laws are precisely specified using set theory
- All instances must satisfy mathematical properties
- Property tests verify law compliance
- Documentation includes mathematical foundations

## Common Patterns

### Smart Constructor Pattern
```haskell
newtype Percent = Percent Double

instance IsSome Double Percent where
  to (Percent d) = d
  maybeFrom d = if d >= 0 && d <= 1 
                then Just (Percent d) 
                else Nothing
```

### Type-directed Conversion
```haskell
-- Convert to specific type
toString = to @String
fromText = from @Text

-- Chain conversions via intermediate types
result = from @Builder $ to input1 <> to input2
```

### Testing Instance Laws
```haskell
-- In test suite
testIs @SourceType @TargetType Proxy Proxy
```

## CI/CD & Workflows

- **GitHub Actions**: `.github/workflows/format-and-test.yaml`
- **Multi-GHC testing**: Tests against GHC 8.8+ and latest
- **Formatting**: Uses reusable workflows for code formatting
- **Documentation**: Auto-generates Haddock docs
- **High test coverage**: 10,000+ QuickCheck iterations in CI

## Troubleshooting

### Build Issues
- Ensure `cabal update` is run for fresh package lists
- Check GHC version compatibility (supports 8.8+)
- Review dependency bounds in `lawful-conversions.cabal`

### Test Failures
- Property test failures usually indicate law violations
- Check mathematical properties of new instances
- Verify injectivity and inverse relationships
- Consider if types truly form subset/superset relationship

### Adding Dependencies
- Keep dependency footprint minimal
- Prefer widely-used, stable packages
- Update version bounds conservatively
- Test across multiple GHC versions

## Related Libraries

This library competes with and complements:
- [isomorphism-class](https://hackage.haskell.org/package/isomorphism-class) - Predecessor library
- [control-iso](https://hackage.haskell.org/package/control-iso) - Control structure isomorphisms
- [type-iso](https://hackage.haskell.org/package/type-iso) - Type-level isomorphisms

The key differentiator is the **lawful typeclass hierarchy** with comprehensive property testing.