---
name: haskell-design
description: Architecture, module layout, effect handling, and testing patterns for Haskell projects in this author's style (devbot / apocrypha / coreutils). TRIGGER when designing a new Haskell project or module, structuring a port to Haskell, deciding how to thread IO/effects, or writing hspec test suites. Complements haskell-dev (which covers tooling — sghci, stack unpack, HLS).
globs: "*.hs"
alwaysApply: false
---

# Haskell Design Patterns

The conventions below are distilled from three reference projects — `devbot` (a task
scheduler daemon), `apocrypha` (a JSON server/client), and `coreutils` (~40 unix utils in
one dispatch binary). They are the house style. When designing a new project or module,
or porting code into Haskell, match these. For *tooling* (type inspection with `sghci`,
reading dep source via `stack unpack`, HLS), see the **haskell-dev** skill — this skill is
about *design*.

The governing philosophy across all three: **pure core, thin IO edge, effects as plain
values, lean dependencies, everything tested.**

---

## 1. Project skeleton

```
<project>.cabal  (or package.yaml for hpack)
stack.yaml                    -- pinned resolver, e.g. lts-24.24 (GHC 9.8/9.10)
stack.yaml.lock
Makefile                      -- all / release / test / format / lint
hie.yaml                      -- HLS cradle (one line: cradle: {stack: ...})
README.md
LICENSE
<Project>/                    -- the LIBRARY lives at the repo root, not under src/
├── <Feature>.hs              -- user-facing / top-level modules
├── <Feature>/
│   ├── Config.hs             -- ADTs + FromJSON + a Valid instance
│   └── Runtime.hs            -- the lifecycle / state machine for that feature
└── Internal/
    ├── Common.hs             -- shared effect synonyms, logger, getTime
    ├── Persist.hs            -- storage wrapper
    └── ...                   -- one focused module per concern
src/
└── main.hs                   -- TINY entry point; dispatches into <Project>.Cli/Run
test/
├── Spec.hs                   -- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
├── Helpers.hs                -- shared stubs/recorders/fixtures (register in cabal!)
└── <Module>Spec.hs           -- one spec file per module
```

Key structural choices:
- **Library source sits at the repo root** under `<Project>/`, with `hs-source-dirs: .`.
  The executable is a separate tiny target under `src/` that depends on the library.
- **`main.hs` is intentionally trivial** — it parses/dispatches and calls into a `Run`
  or `Cli` module. All logic is in the library so it's testable.
- **`Internal/`** holds implementation modules not meant as the public API. Modules
  outside `Internal/` are the surface.

## 2. Cabal / package conventions

Use a `common shared` stanza (cabal) or top-level defaults (hpack) and **repeat
`default-extensions` / `dependencies` / `ghc-options` in every target** — they do NOT
propagate from library to executable to test (a frequent build failure; see haskell-dev).

Standard settings:

```cabal
common shared
  default-language:    GHC2024
  default-extensions:  RecordWildCards        -- + StrictData in coreutils' style
  ghc-options:         -Wall
                       -Wcompat
                       -Wincomplete-record-updates
                       -Wincomplete-uni-patterns
                       -Wredundant-constraints
                       -Wpartial-fields
                       -Wunused-packages       -- keeps the dep list honest
  build-depends:       base >= 4.7 && < 5
```

- **`GHC2024`** language edition: `LambdaCase`, `GADTs`, etc. are on without pragmas.
  `RecordWildCards` is the one extra usually added explicitly. coreutils also defaults
  `StrictData`.
- **Lean dependencies are a value, not an accident.** Before adding a dep, check whether
  an existing one or `base` covers it. `-Wunused-packages` enforces removing dead deps.
  Typical surface: `aeson`/`yaml`, `text`, `bytestring`, `containers`/
  `unordered-containers`, `directory`/`filepath`, `time`, `http-client(-tls)`,
  `process`, `network`, `async`/`stm`. Reach for hand-rolled before heavy (`hmatrix`,
  big frameworks).
- **Adding a module = two edits:** create the file AND register it
  (`exposed-modules` / `other-modules`). Forgetting the cabal entry is the #1 build
  failure. Same for new test specs (`test-suite … other-modules`).
- Optimization via a `release` flag (`-O2 -threaded` on, `-O0` off) so dev builds are
  fast — coreutils' pattern.

## 3. Effect handling — the central pattern

**Inject IO effects as plain function values. No typeclasses, no monad transformers, no
`ReaderT`.** This is the single most important and most distinctive convention.

Each effect is a **type synonym**:

```haskell
type Clock        = IO Integer                       -- POSIX seconds time source
type ContextF     = IO Context                        -- persistence handle factory
type AliveCheck   = Pid -> IO Bool
type Spawner      = String -> [String] -> IO ProcessHandle
type Pinger       = String -> IO ()
```

The effects that **travel together through one pipeline** are bundled into a single
**per-domain env record**, so functions take one argument instead of a growing list:

```haskell
data EventEnv = EventEnv
    { eePinger  :: !Pinger
    , eeOutput  :: !OutputStreamF
    , eeContext :: !ContextF
    , eeClock   :: !Clock
    }

handle :: EventEnv -> Task -> IO Task     -- pipeline entry takes the whole env
```

Rules of thumb:
- **Bundle effects that flow together; keep leaf helpers narrow.** A one-effect helper
  takes just that effect (`flush :: ContextF -> ...`, `monitorShift :: Clock -> ...`),
  not the whole env — handing a full env to a one-effect function hides its real
  dependency.
- The top-level runtime (`Bot.runner`) builds the record **once** with real
  implementations (`httpPing`, `getTime`, `spawnProcess`); **tests build it with stubs.**
- When you add an effect: add a type synonym, thread it into the relevant record(s).
  Reach for the record, not a typeclass — "a record is exactly what `ReaderT` would wrap
  if that ever became necessary."
- **Time** comes from a single `getTime :: Clock` (POSIX seconds, `Integer`), never
  `getCurrentTime` scattered through the code. Logging goes through a `logger`, never raw
  `putStrLn` from inside a runtime.

This keeps the business logic pure and the effectful shell a thin, swappable layer — and
makes testing a matter of passing different functions, not building mocks.

## 4. Data & config style

- **ADTs with strict fields** (`!` on every record field, or `StrictData`). Records use
  `RecordWildCards` for construction/destructuring.
- **Config = ADT + `FromJSON` + `Valid`.** JSON/YAML via `aeson`(+`yaml`). Each config
  type has a hand-written instance: `parseJSON = withObject "Name" $ \o -> ...` with `.:`
  for required and `.:?` (often `.!= def`) for optional fields. Default a `Maybe` field
  in the `FromJSON` instance when absence means a default, so consumers see a plain value.
- A **`Valid` typeclass** centralizes validation (`valid :: a -> Either String a` or
  similar); e.g. optional string fields reject `""`.
- **Wire formats get explicit, commented (de)serializers**, not generic deriving with a
  field-mangler — when JSON field names must match an external spec exactly (an RPC
  protocol, a frontend contract), spell out the mapping and comment why fields are fixed.
- **Adding a field to a record used in positional patterns is a cross-codebase edit.**
  Grep for `TypeName (` and `TypeName [` before trusting the compiler — `Valid` and
  display code often use positional matches the type-checker won't fully catch.

## 5. CLI / dispatch

- `main.hs` stays tiny: parse args, dispatch, exit. coreutils dispatches ~40 utilities
  via a `HashMap` keyed on program name / first arg into an existential `Utility` wrapper
  around a `Util` typeclass (the one sanctioned typeclass — it's a plugin registry, not
  effect injection).
- **Arg parsing:** `System.Console.GetOpt` folding (`foldM`) over a default options
  record. Every tool supports `-h`/`--help`.
- **Errors:** `Either String` for recoverable; `System.Exit.die` for fatal. Don't throw
  for control flow.

## 6. Concurrency (when needed)

Most code is single-threaded. When a daemon needs concurrency (apocrypha's server):
`-threaded`, `async` for spawning, `stm`/`TVar` or `MVar` for shared state. Prefer
thread-per-source (one loop per socket/connection) over reimplementing `select`. Guard
shared mutable state (throttles, caches) with an `IORef` (single-threaded) or `MVar`/STM
(concurrent) — not a global.

## 7. Testing

hspec with **`hspec-discover`** (`test/Spec.hs` is just the discover pragma). One
`<Module>Spec.hs` per module; register each in the cabal `other-modules`.

- **Prefer pure-function unit tests.** The pure core is the bulk of the code and tests
  with direct input/output — no mocks. `QuickCheck` for properties (boundary conditions,
  invariants like "sign of rate matches reported direction"); apocrypha and coreutils
  both use it.
- **Test IO with the effect records, using stubs — not network/HTTP mocks.** Shared
  helpers live in **`test/Helpers.hs`** (register it!). The canonical kit (devbot):
  - `fixedClock n` — a frozen `Clock` so time-dependent arithmetic is exact, not
    wall-clock-dependent.
  - `mkRecorderBy f` / `mkRecorder` — an `IORef`-backed recorder for any injected effect;
    returns a `(record-fn, read-back)` pair. Assert *what would have been written/sent*
    without a real server. **"Don't add a test HTTP server when a recorder will do."**
  - in-memory backends (`getContext ServerMemory`), `noPinger`, `noContext`
    ("must-not-be-read" stub), temp-dir fixtures.
- **Use real IO where it's cheap and deterministic:** `spawnCommand "echo a"`, real temp
  dirs via the `temporary` package, in-memory stores. Reserve integration tests
  (coreutils' `test/integration/*.sh`) for genuinely IO-heavy features; compare output
  against a reference implementation.
- **Golden tests for wire-format contracts:** pin exact bytes/JSON for fixed inputs when
  output must match an external consumer.
- **Capturing stdout:** `System.IO.Silently.capture_` (the `silently` package) — see
  haskell-dev for details. **Timezone-safe filesystem-time tests:** build `UTCTime`
  through the local timezone anchored at noon — also in haskell-dev.

### TDD loop (coreutils house workflow)

Strict red-green-refactor, **one behavioral change per cycle**:
1. **Red:** write a high-level spec test that *runs and produces wrong output* — a
   compile error does not count as a failing test (it blocks all tests and gives no
   behavioral signal). Stub missing functions with `f = undefined` / add record fields
   with defaults so everything *compiles* and only the new test is red. Add focused
   low-level unit tests for the new pure logic.
2. **Green:** minimal code to pass every test, following existing patterns.
3. **Refactor:** with tests green, simplify and extract helpers. `make ready` (format,
   lint, test) must pass before committing.

## 8. Style & hygiene

- 4-space indentation. `where` clauses preferred over top-level helpers when the helper
  isn't reused.
- Qualified imports with short aliases; group imports: stdlib, then external packages,
  then project modules.
- Module headers use Haddock block comments (`Module`, `Description`, `Copyright`,
  `License`, `Maintainer`, `Stability`, `Portability`) for top-level modules.
- Single-sentence Haddock on functions unless something subtle needs explaining.
- **`make format` then `make lint` before committing** — `stylish-haskell --inplace` and
  `hlint -j`. coreutils wraps both plus tests as `make ready`.

## 9. Designing a Python→Haskell port

When porting (the reason this skill often fires):
1. **Inventory by purity.** Split modules into pure (port mechanically: dataclasses → ADTs,
   pure functions → pure functions, `Enum` → sum type) vs IO-edge (needs design).
   Typically the majority is pure and ports almost verbatim — and tests *better*, since
   purity removes guard hacks like "don't write to prod DB during tests."
2. **Find the integration seam** (a database, a file the frontend reads, a socket) where
   Python and Haskell can run side-by-side against the same external state. That seam is
   your incremental migration boundary and your strongest correctness check: same input,
   diff the output.
3. **Port pure core + its tests first** — fully green suite before any socket exists, zero
   risk to the running system.
4. **Then read path, then write path, then the daemon/server**, each shippable and
   verifiable against the seam. Cut over the service last; it's the only
   hard-to-reverse step.
5. **Replace callbacks/globals with effect records** (§3), not a faithful translation of
   Python's callback-passing or module-level singletons.
6. **Numerics:** hand-roll small routines (least-squares fit, percentile) in an
   `Internal/Stats.hs` rather than adding `hmatrix`/`statistics` — but pin them with
   golden fixtures captured from the original (numpy's `polyfit` coefficient order and
   `percentile` interpolation have specific conventions you must reproduce exactly, not
   approximately). Keep a heavier library in mind only as an escape hatch if exact parity
   proves impractical.
7. **Reproduce external wire formats byte-for-byte** with explicit serializers + golden
   tests (RPC JSON, any file a frontend parses, line protocols).
