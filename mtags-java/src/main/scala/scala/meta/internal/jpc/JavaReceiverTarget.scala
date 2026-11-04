package scala.meta.internal.jpc

import java.net.URI

/** Exact source-bound invocation range, in UTF-16 code units. */
final case class JavaReceiverTarget(
    uri: URI,
    sourceSha256: String,
    startUtf16: Int,
    endUtf16: Int
)
