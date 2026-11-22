package scala.meta.internal.metals

import java.util as ju

import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import org.eclipse.lsp4j.Location

case class ImplementationWithStatusResult(
    format: String,
    locations: ju.List[Location],
    isIncomplete: Boolean,
    attemptedDocuments: Int,
    completedDocuments: Int,
    omittedDocuments: Int,
    depthLimitReached: Boolean,
    compilerQuarantined: Boolean,
    omissionReasons: ju.List[String],
)

case class CompileTargetsWithStatusParams(
    targets: ju.List[BuildTargetIdentifier],
    maxTargets: Int,
    timeoutMillis: Long,
)

case class CompileTargetWithStatus(
    uri: String,
    serverName: String,
    serverVersion: String,
    realCompilation: Boolean,
    statusCode: Int,
    reason: String,
)

case class CompileTargetsWithStatusResult(
    format: String,
    targets: ju.List[CompileTargetWithStatus],
    isIncomplete: Boolean,
)
