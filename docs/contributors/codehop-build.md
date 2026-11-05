# Codehop Metals build integration

This fork branch combines upstream `main-v2` at
`85d5cd138a976e7bd6eddbb45add8ecf2a453120` with reviewed fixes and the
compiler extensions used by Codehop. It is the source input for a future
provider image; a source commit does not update an existing image.

## Reviewed fixes

- [#8984](https://github.com/scalameta/metals/pull/8984): Java constructor
  delegation and constant preservation.
- [#8985](https://github.com/scalameta/metals/pull/8985): dependency overload
  identity and compiler failure boundaries.
- [#8986](https://github.com/scalameta/metals/pull/8986): caller-target
  dependency-source selection.
- [#8987](https://github.com/scalameta/metals/pull/8987): source-path/editor-buffer
  identity, cursor handling, stale declaration cleanup and input-cache lifetime.
- [#8988](https://github.com/scalameta/metals/pull/8988): target JAR availability
  and precedence.
- [#8989](https://github.com/scalameta/metals/pull/8989): Java diagnostic source
  attribution and absent positions.
- [#8991](https://github.com/scalameta/metals/pull/8991): refresh signatures when
  a new source replaces classfile-loaded symbols.

The build also includes the current review revisions of
[#8985](https://github.com/scalameta/metals/pull/8985),
[#8986](https://github.com/scalameta/metals/pull/8986), and
[#8987](https://github.com/scalameta/metals/pull/8987), including their use of the
existing test infrastructure. Source registration, edits, and re-entry after
eviction or pruning advance the typer epoch; an unchanged retained source or
cursor movement preserves it. The integration lifecycle test checks both sides
of this combined contract.

## Retained local extensions

The local batch SemanticDB v2 endpoint and target-classpath selection are
required by the current Codehop adapter. Their public upstream contracts remain
proposals in [#473](https://github.com/scalameta/metals-feature-requests/issues/473)
and [#474](https://github.com/scalameta/metals-feature-requests/issues/474).
Strict compiler selection applies to explicit compiler-document requests;
ordinary reference indexing retains upstream's MBT fallback policy.

The branch also retains compiler receiver/callable/constructor evidence,
compiled Java origins and dependency bodies, compiled classpath directories,
Java symbol-language metadata, bounded logging/report retention, and the
constructor isolation/cached-source-error revalidation repair. Existing
Codehop isolation fixtures have separate class names so the reviewed upstream
fixtures also run.

Navigation and SemanticDB synthetic serialization share current-overload
rebinding. A cached method missing from its owner's current declarations may
rebind only through unique compiler type equality; unmatched or ambiguous
methods do not borrow the first overload's identity. The
[overload identity fixture](../../tests/cross/src/test/scala/tests/pc/SemanticdbOverloadIdentitySuite.scala)
also checks previously serialized symbols after declaration replacement. The
callable LSP fixture checks complete evidence again after opening its declaration.

Pruned public-symbol SemanticDB requests invalidate the cached unit's full-body
state. Re-entering that source advances the typer epoch and rebuilds its body;
clean diagnostics on a pruned document do not establish callable availability.
The [callable lifecycle fixture](../../tests/unit/src/test/scala/tests/ScalaCallableEvidenceLifecycleLspSuite.scala)
interleaves the same pruning request with navigation, hover, signature help and
full callable export, alongside an unpruned control.

The obsolete interactive-cache concurrency writeup associated with closed
[#8789](https://github.com/scalameta/metals/pull/8789) is excluded. The older
single-case Turbine fixture is superseded by the reviewed generated-JAR suite.

## Integration validation

The synchronized source passes 262 compiler tests on Scala 2.13.18, 227 on
Scala 2.12.21, 135 server/LSP tests, and 73 Java PC tests on Corretto 21.0.5.
Existing suite conditions report 16 and 23 ignored cross-compiler cases,
respectively. Changed-source scalafmt and scalafixAll pass. The first-request
regression checks compile the fixture before querying either source and cover
both source orders, repeat stability, and genuine source errors.

Diagnostic-only instrumentation and opt-in investigation probes remain outside
the committed build input. Package a clean checkout of the committed tree and
retain the compiler-JAR and image identities with the build artifacts.

## Building

Use a clean checkout of `codehop/build-main-v2-20261104` from `REASY/metals`.
Record its exact `git rev-parse HEAD` and pass that value as
`METALS_SOURCE_REVISION` to the Codehop `metals-dist` build. The build fetches
that commit from the fork; its older default revision does not select this
branch automatically. Publish the source commit to the fork before building.

Use a JDK 21 build environment. The provider distribution needs both Scala
2.13.18 and 2.12.21 compiler artifacts, as well as the Java presentation
compiler and the custom interfaces from this same source tree. Package against
a fresh build-local Maven seed, exclude ambient Ivy snapshots, and verify the
embedded compiler JAR hashes against that seed. This avoids the stale snapshot
selection previously observed during launcher packaging.

Before using an image in a full repository run, record the source commit,
launcher/compiler hashes, image digest and architecture. Run the bounded live
startup/import/readiness, cross-file semantic evidence, source-immutability and
cleanup checks against that exact image. Source-level test results alone do
not establish image contents or repository-wide resolution gains.
