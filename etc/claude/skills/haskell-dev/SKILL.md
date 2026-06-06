---
name: haskell-dev
description: Haskell development tools — type inspection with sghci, reading dependency source/docs locally via stack unpack. TRIGGER when working on Haskell code, exploring types, or needing library documentation.
globs: "*.hs"
alwaysApply: false
---

# Haskell Development Tools

## Type Inspection with sghci

`sghci` is a shell script that runs ghci non-interactively via stack. Use it to check types, kinds, instances, and evaluate expressions without entering a REPL.

```
sghci <module-file> '<ghci-commands>'
```

### When to use
- Checking the type of a function or expression: `:t functionName`
- Checking composed types: `:t f . g . h`
- Inspecting a type/class: `:i TypeName`
- Verifying a refactored expression type-checks: `:t \x -> newExpr x`
- Checking what instances exist: `:i SomeClass`

### Examples
```sh
# Single query
sghci Coreutils/Nl.hs ':t process'

# Multiple queries — separate with literal newlines
sghci Coreutils/Nl.hs ':t worker
:t process
:i NlState'

# Check a composed pipeline's type
sghci Coreutils/Rev.hs ':t Q.unlines . S.subst Q.chunk . S.map C.reverse . mapped Q.toStrict . Q.lines'

# Verify a lambda expression type-checks
sghci Coreutils/Nl.hs ':t \ref l -> Data.IORef.atomicModifyIORef'\'' ref (\st -> execute st l)'
```

### Tips
- The file argument determines which modules are in scope — use the file you're working on
- Output goes to stdout; stderr is suppressed (`2>/dev/null`)
- If you need a function from another module, use the fully qualified name (e.g. `Data.IORef.newIORef`)
- Use `:i` (info) instead of `:t` (type) when you want to see constructors, instances, or where something is defined

## Project-wide diagnostics with HLS

If `haskell-language-server-wrapper` is on PATH (symlinked into `~/.local/bin`), its
headless check mode typechecks the whole project using the *same* analysis the editor
uses — usually faster than `stack build` because it skips code generation:

```sh
haskell-language-server-wrapper .            # whole project
haskell-language-server-wrapper src/Foo.hs   # one file + its dependencies
```

Exit 0 with no output means clean; diagnostics print to stdout. The first run on a
cold cache is slow (it builds the stack session via the project's `hie.yaml` cradle);
later runs are fast.

HLS binaries must match the project's GHC exactly. If the wrapper says `Failed to find
a HLS version for GHC <x>`, run `install-hls` in the project to fetch a matching build
(see `dotfiles/bin/install-hls`).

Use this for a quick "does the whole project still typecheck" pass. For a single
expression's type use `sghci` above; for a real build or to run tests, use `stack`.

## Reading Dependency Source & Documentation

Stack does NOT keep package source or HTML docs locally after building. To read library source (which contains inline haddock documentation):

```sh
stack unpack <package-name> --to /tmp/haskell-src
```

### When to use
- Understanding how a library function works internally
- Reading haddock documentation for a dependency without a browser
- Exploring what functions a module exports
- Debugging unexpected behavior from a library

### Key packages and their source layout

After unpacking, the source files with docs are at:

| Package | Source path |
|---------|------------|
| `streaming` | `/tmp/haskell-src/streaming-<ver>/src/Streaming/Prelude.hs` |
| `streaming-bytestring` | `/tmp/haskell-src/streaming-bytestring-<ver>/lib/Streaming/ByteString/Char8.hs` |

To find the right file in an unfamiliar package:
```sh
stack unpack <package> --to /tmp/haskell-src
find /tmp/haskell-src/<package>-*/ -name '*.hs' | head -20
```

### Tips
- `/tmp` is cleared on reboot — just re-run `stack unpack` if needed
- The source contains full haddock comments (`-- |` blocks) which are the same content as the HTML docs on Hackage
- Check the version that's actually in use: `sghci SomeFile.hs ':i SomeType'` shows the package version in the "Defined in" line

## `package.yaml` targets are independent

`default-extensions`, `dependencies`, and `ghc-options` do NOT propagate from `library` to `executables` to `tests`. Each target stanza has its own copies. Symptom: code that compiles in the library fails in the executable with `Illegal \case` (LambdaCase missing) or `Could not load module` (dep missing).

```yaml
library:
  default-extensions: [LambdaCase, RecordWildCards]
  dependencies: [base, time]

executables:
  myprog:
    # ↓ must be repeated; not inherited from library above
    default-extensions: [LambdaCase]
    dependencies: [base, myproj, time]
```

When you add an extension or dep to fix a build error, check whether you need to add it to *each* target that uses it.

## Time-zone-safe filesystem tests

Tests that touch `getModificationTime` / `setModificationTime` are flaky across timezones if you build `UTCTime`s naïvely. A test that hardcodes `UTCTime (fromGregorian 2020 9 30) 0` (UTC midnight) will round-trip to `2020-09-29` in `Pacific/Auckland` and break the test.

Fix: build the `UTCTime` *through* the local timezone, anchored at noon so any TZ shift stays on the same calendar day:

```haskell
import Data.Time           (getCurrentTimeZone)
import Data.Time.Calendar  (fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), localTimeToUTC)

localNoon :: Integer -> Int -> Int -> IO UTCTime
localNoon y m d = do
    tz <- getCurrentTimeZone
    pure (localTimeToUTC tz (LocalTime (fromGregorian y m d) (TimeOfDay 12 0 0)))
```

Use this when you need a file's mtime to round-trip cleanly to a specific *local* calendar day — e.g. testing code that calls `localtime()` or `utcToLocalTime` to extract month/day.

## Capturing stdout in tests

`silently` (in LTS, separate package) lets hspec tests assert on what a function prints:

```haskell
import System.IO.Silently (capture_)

it "prints a greeting" $ do
    output <- capture_ $ greet "world"
    output `shouldBe` "hello, world\n"
```

`capture_` discards the action's return value and gives you stdout. `capture` returns `(stdout, a)` if you need the value too. There's also `silence` to suppress stdout without capturing it.

Add `silently` to the test target's `dependencies` in `package.yaml`. The `coreutils` test suite uses this pattern throughout.
