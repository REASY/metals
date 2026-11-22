---
id: java-dependency-body-prototype
title: Java dependency-body capability prototype
---

The Java presentation compiler's `javaDependencyBodiesExperimental` method is a
bounded capability probe with schema
`metals-java-dependency-bodies.experimental.v1`. The separate service route
`metals/javaDependencyBodies` wraps it in `metals-java-dependency-bodies.v2`.
It does not change `metals/javaReceiverTypes`. Codehop's separately versioned
dependency compiler-body profile can admit possible bodies only after exact
context, selected-contract, package and supporting-response correlation.

The service validates the exact owning BSP target and source hash, creates a
fresh compiler with evaluated project javac options, and shuts it down after the
request. It retains original/effective compiler-option identities, the complete
ordered compiler classpath and its hash, and the explicit inventory limit.
Classpath identity is bounded to 4,096 entries and 128 KiB of path characters.
The request has two phases. `FREEZE` requires null classpath/content hashes
and records ordered content identities for all actual compiler inputs, including
provider-client output directories. `ACQUIRE` requires those frozen
`compilerClasspathSha256` and `compilerContentSha256` values. A path mismatch
returns `CLASSPATH_CHANGED`; content drift before or after compiler work returns
`CONTEXT_CHANGED`. The content manifest consists of ordered
`[path, FILE|DIRECTORY, sha256, entryCount, byteCount]` rows. Directory hashes
cover sorted relative names and file-content hashes, including empty directories.
Symlinks and unsupported/unreadable entries fail explicitly. Checks are cancellable
and bounded to 65,536 filesystem entries, 1 GiB content and 8 MiB of member names.
The service does not establish readiness on behalf of the caller. This is
target/batch context work, never a per-callsite API lookup or completeness proof.

## Authority and scope

The separate `metals/javaReceiverTypes` route uses
`metals-java-receiver-types.v2`. It retains explicit compiler-selected owner,
binary owner, member, erased JVM descriptor and abstract role independently of
the receiver type. Constructor and implicit-invocation `SELECTED` rows have no
invented value receiver; constructors use `NEW_CLASS` and `<init>`. Method rows
retain the original receiver evidence where representable. Exact source ranges,
source hashes, target and compiler options remain required. This selection
profile supplies neither implementation witnesses nor closed-world completeness.

The caller supplies source-hashed, exact UTF-16 invocation targets against a
fresh compiler session with a frozen target, classpath and compiler options.
The response retains independent selected-declaration and receiver evidence,
adding `dependencyBodies` only for typed receivers.

Jar entries provide candidate names, not semantic owners. Javac must return the
matching binary identity and classfile origin before artifact association. For
each concrete, fully instantiated candidate receiver, javac must prove both
`Types.isSubtype(candidate, actualReceiver)` and
`Elements.overrides(body, selectedContract, candidate)`. Name lookup only narrows
the request-owned index. No annotation or source attachment is required. Native,
abstract, private and static methods cannot be body candidates. Each overloaded
method must satisfy the compiler override relation independently.

Binary lookup uses the active javac class finder in its unnamed classpath module
(or pre-module compiler scope), never a `$`-to-dot conversion. Literal dollar
names and compiler-generated nesting are distinct from source member lookup.
Anonymous/local classes remain excluded. Named-module dependency discovery is
outside this classpath-only prototype.

A body retains the compiler owner, binary owner, member, erased descriptor,
concrete receiver witness, and separate content-addressed body/witness artifacts.
The selected artifact is retained independently. Content hashes are checked
again before returning. These artifact coordinates are not package identities:
future LSP admission must join each independently to frozen dependency metadata
and retain provider context and response. Never infer owners or packages from
jar names.

The prototype does not solve generic candidate-class instantiation, method type
parameters, raw/wildcard receivers, directory-classpath
enumeration, anonymous/local binary names that javac cannot map, or duplicate
binary identities. Omissions are explicit. The broader compiler context must
be frozen by the caller; hashing returned artifacts alone cannot validate a
reused compiler epoch.

Multi-release jar entries supply a deduplicated union of binary lookup
candidates from base and versioned trees. They do not select the effective
version: the frozen javac target and file manager do that. Future-only names
unavailable to that compiler target supply no body evidence. Artifact hashes
cover the entire jar, including every version; the same binary name in separate
classpath artifacts remains ambiguous. Fixtures distinguish inherited physical
body owners under targets 8, 17 and 21 on a JDK 21 provider runtime.

## Bounds and interpretation

- At most 4,096 distinct classpath entries and 1 GiB of artifact bytes.
- At most 8,192 jar entries by default; explicit requests may choose 1–262,144.
- At most 16,777,216 receiver-compatibility checks to select relevant catalog types.
- At most 1,048,576 indexed members and 1,048,576 per-batch compatibility checks.
- At most 16 distinct body identities per callsite.
- Existing receiver source, request, cancellation and output limits apply.

Inventory construction, hashing and method indexing are request-owned setup,
not per-callsite filesystem scans. Cancellation is checked during hashing,
enumeration, compiler lookup and compatibility checks. Unsupported inputs,
budgets, mutation and compiler errors cannot supply closure. `SCANNED` describes
the operation, not semantic completeness. `populationClosed` is always false;
zero bodies means unknown, not proof that no implementation exists.

The member catalog includes only witnesses javac proves compatible with at least
one exact requested receiver. Unrelated dependency classes do not consume its
retained-member budget. This prefilter grants no body authority; every emitted
method still needs the independent per-callsite subtype and override proof.
Receiver-prefilter exhaustion is visible as `RECEIVER_INVENTORY_BUDGET`.
Catalog order follows frozen classpath and jar-entry order, so expanding an
inventory does not move new alphabetically earlier classes before its existing
prefix. Later-discovered duplicate identities can still invalidate ambiguous
artifact mappings; a larger budget is not an unconditional monotonicity claim.

Before production canonical integration, require representative applicability measurement,
frozen-context correlation, versioned package mappings, canonical record/replay
admission, and live readiness/immutability/cleanup validation. A public fixture
pass alone does not establish corpus-level recovery.

Focused verification:

```text
sbt --client 'javapc/testOnly pc.JavaDependencyBodiesSuite pc.JavaReceiverTypesSuite pc.JavaReceiverTargetsSuite pc.JavaReceiverSemanticSuite'
sbt --client 'unit/testOnly tests.JavaDependencyLspSuite tests.JavaReceiverLspSuite'
```
