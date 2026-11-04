# Scala callable evidence export

`metals/scalaCallableEvidence` accepts `SemanticdbTextDocumentsParams` (`uris`)
and returns the separately versioned `metals-scala-callable-evidence.v1` shape.
It does not change `metals-semanticdb-text-documents.v2`.

The caller must complete semantic readiness against a frozen workspace before
using answers as semantic evidence. The endpoint requires build-target
presentation compilers and classpaths, disables fallback compilers, propagates
cancellation, and requires an exact returned-document census. Each document
retains its URI and compiler MD5. Consumers must compare these with their frozen
source content; a URI is not ownership evidence.

`AVAILABLE` at the result level means export completed. Documents can separately
be `UNAVAILABLE`, and callable signatures can be `UNSUPPORTED_SIGNATURE`.
Neither an available response nor an empty selection list establishes complete
callsite coverage or execution closure.

Selections retain an exact range and SemanticDB symbol only for root synthetics
of shape `SelectTree(OriginalTree(range), IdTree(symbol))` with equal original
and synthetic ranges. Unsupported synthetic shapes are counted explicitly.
No display-name resolution or synthetic-tree guessing is performed.

Method signatures retain symbol, display name, kind, properties, ordered
parameter clauses, parameter identities/properties and structured return types.
Types support `type_ref`, `single_type`, `this_type`, `by_name` and `repeated`.
References preserve symbols, optional prefixes and ordered type arguments.
Generic method parameters, hardlinked scopes, absent parameter symbols and
unsupported types fail the whole signature: no truncated positive formal vector
or return type is exposed. Selected symbols and signatures may be in different
requested documents; consumers must join their exact identities and preserve
ambiguities and contradictions.

Bounds are 32 unique absolute local file URIs (8,192 characters each), 20,000
symbols and 4,096 synthetics per document, and an 8-MiB encoded response. Methods
allow 16 clauses and 128 parameters per clause; type projection allows 256 nodes
per method, depth 16 and 128 type arguments per reference. Bounds return explicit
unavailability or unsupported signatures rather than silently truncating facts.
Compiler errors, missing source identity and duplicate symbol records make the
document unavailable. Raw compiler acquisition has a 15-second timeout.

Tests cover real presentation-compiler signatures and selected-apply synthetics,
qualified homonymous types, authored overloads, missing parameters, unsupported
types, incorrect synthetic ranges, compiler failures and request/fact bounds.
An optional unit-test property, `codehop.rawSemanticdbInput`, projects a retained
public raw compiler protobuf through the actual exporter. This is a capability
test, not downstream canonical admission or a population-quality claim.
