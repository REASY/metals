package scala.meta.internal.metals.mbt

import java.time.Duration
import java.util.Collections
import java.util.LinkedHashMap as JLinkedHashMap
import java.util.concurrent.CancellationException
import java.util.concurrent.TimeoutException

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

import scala.meta.infra.Event
import scala.meta.infra.MonitoringClient
import scala.meta.internal.async.CompletableCancelToken
import scala.meta.internal.metals.AdjustRange
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.Compilers
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.ImplementationsResult
import scala.meta.internal.metals.MetalsEnrichments.*
import scala.meta.internal.metals.ReferencesResult
import scala.meta.internal.metals.SymbolAlternatives
import scala.meta.internal.metals.Synthetics
import scala.meta.internal.metals.TaskProgress
import scala.meta.internal.metals.Time
import scala.meta.internal.metals.Timer
import scala.meta.internal.metals.WorkDoneProgress
import scala.meta.internal.metals.clients.language.MetalsLanguageClient
import scala.meta.internal.metals.config.UserConfiguration
import scala.meta.internal.metals.noAdjustRange
import scala.meta.internal.mtags.MD5
import scala.meta.internal.mtags.Semanticdbs
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.semanticdb as s
import scala.meta.internal.semanticdb.Scala.*
import scala.meta.internal.semanticdb.Synthetic
import scala.meta.internal.semanticdb.TypeRef
import scala.meta.internal.semanticdb.XtensionSemanticdbSymbolInformation
import scala.meta.io.AbsolutePath
import scala.meta.pc.CancelToken

import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.ReferenceParams
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither

class MbtReferenceProvider(
    mbt: MbtWorkspaceSymbolProvider,
    compilers: Compilers,
    buffers: Buffers,
    semanticdbs: () => Semanticdbs,
    buildTargets: BuildTargets,
    time: Time,
    languageClient: MetalsLanguageClient,
    workDoneProgress: WorkDoneProgress,
    metrics: MonitoringClient,
    workspace: AbsolutePath,
    userConfig: () => UserConfiguration,
)(implicit ec: ExecutionContext) {
  private val cache =
    new TextDocumentCache(() => userConfig().mbt.semanticdbCacheMaxSize)

  // When looking for usages of a method, we don't visit supermethods from these
  // types because it would result in a ton of noisy results. This list should
  // not enumerate ad-hoc cases like `java.io.Serializable`, we should only
  // enumerate cases where it will confuse the user if they get matching
  // results. These types are automatically made parents of all classes and
  // Scala case classes so people don't even realize their type if extending,
  // for example, `scala.Product`.
  private val ignoredSuperSymbols = Set(
    "java/lang/Object#", "scala/Any#", "scala/AnyRef#", "scala/Product#",
    "scala/Serializable#",
  )

  private val groupSize = 100

  private def isMbtMode: Boolean =
    buildTargets.all.exists { target =>
      buildTargets.buildServerOf(target.getId).exists(_.isMbt)
    }

  /**
   * Groups candidate paths for indexing. In MBT mode, files are batched by a
   * fixed size. In BSP mode, files are grouped by build target so each batch uses
   * the correct per-target presentation compiler.
   */
  private def groupPathsForIndexing(
      paths: Iterable[AbsolutePath]
  ): Iterator[Seq[AbsolutePath]] =
    if (isMbtMode) paths.iterator.grouped(groupSize)
    else
      paths.toSeq
        .groupBy(buildTargets.inverseSources(_))
        .toSeq
        .sortBy(_._1.map(_.getUri).getOrElse(""))
        .map(_._2)
        .iterator

  /** Returns the length of the common path prefix between two paths. */
  private def commonPrefixLength(a: AbsolutePath, b: AbsolutePath): Int = {
    val aStr = a.toString
    val bStr = b.toString
    var i = 0
    val minLen = math.min(aStr.length, bStr.length)
    while (i < minLen && aStr.charAt(i) == bStr.charAt(i)) {
      i += 1
    }
    i
  }

  /**
   * Query MBT for possible references, filtering out proto files and sorting
   *  results by proximity to the given path.
   */
  private def sortedCandidates(
      params: MbtPossibleReferencesParams,
      path: AbsolutePath,
  ): Seq[AbsolutePath] =
    mbt
      .possibleReferences(params)
      .filterNot(_.filename.endsWith(".proto"))
      .toSeq
      .sortBy(c => (-commonPrefixLength(path, c), c.toString))

  // We timebox the search because the user could do "Find References" on
  // java.lang.String and get matches in ALL files, which is always going to
  // take a long time to compute. We still stream results for clients that
  // support partial results so the user should see results much faster than
  // this timeout.
  private def timeout: FiniteDuration =
    if (sys.props.contains("metals.debug")) 20.minutes
    else userConfig().mbt.referencesTimeoutSeconds.seconds

  private val protobufReferences = new MbtProtobufReferenceProvider(
    mbt,
    buffers,
    languageClient,
    time,
    groupSize,
    userConfig,
  )

  private def compilerQuarantined(path: AbsolutePath): Boolean =
    compilers
      .semanticdbCompilerContext(path, isMbtMode)
      .exists(_.semanticdbCompilerQuarantined())

  def implementations(
      params: l.TextDocumentPositionParams
  ): Future[ImplementationsResult[l.Location]] =
    implementations(params, EmptyCancelToken)

  def implementations(
      params: l.TextDocumentPositionParams,
      token: CancelToken,
  ): Future[ImplementationsResult[l.Location]] = implementations(
    params.getTextDocument.getUri.toAbsolutePath,
    params.getPosition(),
    createOutput = (location, info) => location,
    token = token,
  )

  def implementations[T](
      path: AbsolutePath,
      pos: l.Position,
      createOutput: (l.Location, s.SymbolInformation) => T,
  ): Future[ImplementationsResult[T]] =
    implementations(path, pos, createOutput, EmptyCancelToken)

  def implementations[T](
      path: AbsolutePath,
      pos: l.Position,
      createOutput: (l.Location, s.SymbolInformation) => T,
      token: CancelToken,
  ): Future[ImplementationsResult[T]] =
    workDoneProgress.trackProgressFuture(
      "Finding implementations",
      taskProgress => {
        val timer = new Timer(time)
        Future(doImplementations(path, pos, taskProgress, createOutput, token))
          .map { case (results, symbol) =>
            metrics.recordEvent(
              Event
                .duration("find_implementations", timer.elapsed)
                .withLanguage(path.toJLanguage)
                .withLabel("resultCount", results.results.size.toString)
                .withOptional("symbol", symbol)
            )
            results
          }
      },
    )

  private def doImplementations[T](
      path: AbsolutePath,
      pos: l.Position,
      taskProgress: TaskProgress,
      createOutput: (l.Location, s.SymbolInformation) => T,
      token: CancelToken,
  ): (ImplementationsResult[T], Option[String]) = {
    val timer = new Timer(time)
    token.checkCanceled()
    val requestDoc = cache.indexSingle(path, token)
    if (requestDoc.md5.isEmpty) {
      return (
        ImplementationsResult(
          Nil,
          isIncomplete = true,
          totalCandidates = 1,
          attemptedDocuments = 1,
          compilerQuarantined = compilerQuarantined(path),
          omissionReasons = List("REQUEST_DOCUMENT_UNAVAILABLE") ++
            Option.when(compilerQuarantined(path))("COMPILER_QUARANTINED"),
        ),
        None,
      )
    }
    val enclosingOccurrences = this.enclosingOccurrences(requestDoc, pos)
    scribe.info(
      s"implementations: found ${enclosingOccurrences.length} occurrences"
    )

    if (path.isProtoFilename) {
      protobufReferences.implementations(
        path,
        requestDoc,
        enclosingOccurrences,
        taskProgress,
        timer,
        paths => cache.indexWithToken(paths, token),
        commonPrefixLength,
        createOutput,
      )
    } else {
      doScalaJavaImplementations(
        path,
        enclosingOccurrences,
        taskProgress,
        timer,
        createOutput,
        token,
      )
    }
  }

  private def doScalaJavaImplementations[T](
      path: AbsolutePath,
      enclosingOccurrences: Seq[s.SymbolOccurrence],
      taskProgress: TaskProgress,
      timer: Timer,
      createOutput: (l.Location, s.SymbolInformation) => T,
      token: CancelToken,
  ): (ImplementationsResult[T], Option[String]) = {
    val enclosingSymbols = enclosingOccurrences.map(_.symbol)
    val primarySymbol = enclosingSymbols.headOption
    val isOverridenSymbol = mutable.Set.from(enclosingSymbols)
    val isVisitedMethodName = mutable.Set.empty[String]
    var lastQueryRound = Set.empty[String]
    val result = mutable.ListBuffer.empty[T]
    val isVisitedURI = mutable.Set.empty[String]
    var processedCandidates = 0
    var totalCandidates = 0
    var timedOut = false
    var depthLimitReached = false
    var hadDocumentErrors = false
    var hadQuarantinedCompiler = false
    val candidatePaths = mutable.Set.empty[AbsolutePath]
    val attemptedPaths = mutable.Set.empty[AbsolutePath]
    val completedPaths = mutable.Set.empty[AbsolutePath]

    def visitDoc(doc: s.TextDocument): Boolean = {
      def overridesOrImplements(info: s.SymbolInformation): Boolean = {
        if (
          info.language.isScala
          && (info.kind.isClass || info.kind.isTrait || info.kind.isObject)
        ) {
          info.signature match {
            case s.ClassSignature(_, parents, _, _) =>
              parents.exists {
                case TypeRef(_, symbol, _) => isOverridenSymbol(symbol)
                case _ => false
              }
            case _ => false
          }
        } else {
          info.overriddenSymbols.exists(sym => isOverridenSymbol(sym))
        }
      }

      if (isVisitedURI.contains(doc.uri)) {
        return false
      }
      isVisitedURI += doc.uri
      // prebuild occurrences map to avoid iterating over the occurrences more than once
      lazy val occurrencesForSymbol = doc.occurrences.groupBy(_.symbol)
      for {
        info <- doc.symbols.iterator
        if overridesOrImplements(info)
        occ <- occurrencesForSymbol(info.symbol).iterator
        if occ.role.isDefinition
        range <- occ.range.toList
      } {
        if (info.kind.isMethod) {
          isVisitedMethodName += info.displayName
        }
        isOverridenSymbol += occ.symbol
        result += createOutput(range.toLocation(doc.uri), info)
      }
      true
    }
    val maxDepth = 10
    @tailrec
    def loop(depth: Int): Unit = {
      if (depth > maxDepth) {
        depthLimitReached = true
        scribe.warn(s"implementation: depth reached: $depth")
        return
      }
      val toQuery = isOverridenSymbol.toSeq.filterNot { s =>
        val sym = Symbol(s)
        if (sym.isMethod) isVisitedMethodName.contains(sym.displayName)
        else lastQueryRound.contains(s)
      }
      val candidates = sortedCandidates(
        MbtPossibleReferencesParams(implementations = toQuery),
        path,
      )

      candidatePaths ++= candidates
      scribe.info(s"Found ${candidates.size} candidate files for depth $depth.")
      lastQueryRound = Set.from(isOverridenSymbol)
      var didMakeProgress = false
      processedCandidates = 0
      totalCandidates = candidates.size
      val remaining = groupPathsForIndexing(candidates)
      while (remaining.hasNext && !timer.hasElapsed(timeout)) {
        token.checkCanceled()
        val paths = remaining.next()
        attemptedPaths ++= paths
        val indexed = cache.indexWithToken(paths, token)
        hadQuarantinedCompiler ||= paths.exists(compilerQuarantined)
        for (doc <- indexed.documents) {
          token.checkCanceled()
          doc.uri.toAbsolutePathSafe.foreach(completedPaths.add)
          hadDocumentErrors ||= doc.diagnostics.exists(
            _.severity == s.Diagnostic.Severity.ERROR
          )
          val didVisit = visitDoc(doc)
          didMakeProgress = didMakeProgress || didVisit
          processedCandidates += 1
          taskProgress.update(
            processedCandidates,
            totalCandidates,
            Some(s"Processing ${doc.uri.toString.split("/").last}"),
          )
        }
      }
      timedOut = remaining.hasNext
      if (timedOut) {
        scribe.warn(
          s"Time out analyzing candidate files at $processedCandidates/${totalCandidates}."
        )
      } else if (didMakeProgress) {
        loop(depth = depth + 1)
      }
    }

    // visitDoc(requestDoc)
    loop(depth = 0)

    scribe.info(
      s"implementations: found ${result.size} implementation results in $timer"
    )

    val missingDocuments = candidatePaths.exists(path => !completedPaths(path))
    val isIncomplete =
      timedOut || depthLimitReached || hadDocumentErrors || hadQuarantinedCompiler ||
        enclosingOccurrences.isEmpty || missingDocuments
    (
      ImplementationsResult(
        result.toList,
        isIncomplete,
        completedPaths.size,
        candidatePaths.size,
        attemptedPaths.size,
        depthLimitReached,
        hadQuarantinedCompiler,
        List(
          Option.when(timedOut)("SEARCH_TIMEOUT"),
          Option.when(depthLimitReached)("DEPTH_LIMIT"),
          Option.when(hadDocumentErrors)("DOCUMENT_DIAGNOSTICS"),
          Option.when(hadQuarantinedCompiler)("COMPILER_QUARANTINED"),
          Option.when(enclosingOccurrences.isEmpty)("SYMBOL_UNAVAILABLE"),
          Option.when(missingDocuments)("DOCUMENT_UNAVAILABLE"),
        ).flatten,
      ),
      primarySymbol,
    )
  }

  def enclosingOccurrences(
      queryRange: l.Position,
      path: AbsolutePath,
  ): Seq[s.SymbolOccurrence] = {
    val requestDoc = cache.indexSingle(path)
    this.enclosingOccurrences(requestDoc, queryRange)
  }

  def textDocument(
      path: AbsolutePath
  ): s.TextDocument = {
    cache.indexSingle(path)
  }

  /**
   * Find all method reference occurrences within a given range.
   * Used for outgoing calls to find all methods called within a method body.
   * Returns tuples of (referenceOccurrence, Option[definitionLocation]).
   */
  def methodReferencesInRange(
      path: AbsolutePath,
      enclosingRange: l.Range,
  ): Seq[(s.SymbolOccurrence, Option[l.Location])] = {
    val doc = cache.indexSingle(path)
    val definitions = doc.occurrences
      .filter(occ => occ.role.isDefinition && occ.symbol.endsWith(")."))
      .flatMap(occ => occ.range.map(r => occ.symbol -> r.toLocation(doc.uri)))
      .toMap
    doc.occurrences
      .filter { occ =>
        occ.symbol.isGlobal &&
        occ.symbol.endsWith(").") &&
        occ.role.isReference &&
        occ.range.exists(range => enclosingRange.encloses(range.toLsp))
      }
      .map { occ =>
        (occ, definitions.get(occ.symbol))
      }
  }

  def textDocuments(paths: Seq[AbsolutePath]): s.TextDocuments = {
    val documents = for {
      group <- groupPathsForIndexing(paths.distinct.sortBy(_.toString))
      document <- cache.index(group).documents
    } yield document
    s.TextDocuments(documents = documents.toSeq.sortBy(_.uri))
  }

  def compilerTextDocuments(paths: Seq[AbsolutePath]): s.TextDocuments =
    compilerTextDocuments(paths, EmptyCancelToken)

  def compilerTextDocuments(
      paths: Seq[AbsolutePath],
      token: CancelToken,
  ): s.TextDocuments = {
    token.checkCanceled()
    val documents = for {
      group <- groupPathsForIndexing(paths.distinct.sortBy(_.toString))
      document <- {
        token.checkCanceled()
        cache.indexCompilerOnly(group, token).documents
      }
    } yield document
    token.checkCanceled()
    s.TextDocuments(documents = documents.toSeq.sortBy(_.uri))
  }

  private def batchCompileTextDocuments(
      paths: Seq[AbsolutePath],
      useFallbackCompiler: Boolean,
      requireBuildTargetClasspath: Boolean = false,
      token: CancelToken = EmptyCancelToken,
  ): Seq[s.TextDocument] = {
    token.checkCanceled()
    val operation = new CompletableCancelToken()
    token.onCancel.whenComplete { (cancelled, _) =>
      if (cancelled) operation.cancel()
    }
    try {
      Await
        .result(
          compilers.batchSemanticdbTextDocuments(
            paths,
            operation,
            // Cooperative batch budget; PC jobs have a separate 45s hard bound.
            Duration.ofSeconds(15),
            useFallbackCompiler = useFallbackCompiler,
            requireBuildTargetClasspath = requireBuildTargetClasspath,
          ),
          65.seconds,
        )
        .documents
    } catch {
      case e @ (_: CancellationException | _: TimeoutException) =>
        operation.cancel()
        token.checkCanceled()
        scribe.warn(
          s"references: indexing ${paths.size} documents failed.",
          e,
        )
        scribe.debug(s"indexed: ${paths.mkString(", ")}}", e)
        Seq.empty[s.TextDocument]
    }
  }

  def references(
      params: ReferenceParams,
      findRealRange: AdjustRange = noAdjustRange,
      includeSynthetics: Synthetic => Boolean = _ => true,
      withDefinitionFallback: Option[String] = None,
  ): Future[List[ReferencesResult]] = {
    val timer = new Timer(time)
    val path = params.getTextDocument.getUri.toAbsolutePath
    val queryRange = params.getPosition()
    val requestDoc = cache.indexSingle(path)
    val semanticdbOccurrences =
      this.enclosingOccurrences(requestDoc, queryRange).map(_.symbol)
    val enclosingSymbols =
      if (semanticdbOccurrences.isEmpty) withDefinitionFallback.toSeq
      else semanticdbOccurrences
    scribe.info(
      s"references: found ${enclosingSymbols.length} occurrences"
    )
    // If all symbols are local, use compilers.references directly
    val allLocal = enclosingSymbols.forall(_.isLocal)
    val results =
      if (allLocal && requestDoc.language.isScala) {
        scribe.debug("references: all local, using compilers.references")
        compilers
          .references(params, EmptyCancelToken, findRealRange)
      } else {
        workDoneProgress.trackProgressFuture(
          "Finding references",
          taskProgress =>
            Future {
              doReferences(
                timer,
                params,
                requestDoc,
                enclosingSymbols,
                taskProgress,
                params.getContext().isIncludeDeclaration(),
                findRealRange,
                includeSynthetics,
              )
            },
        )
      }

    results.map { refs =>
      val resultCount = refs.iterator.map(_.locations.size).sum
      val sym = refs.map(_.symbol).headOption
      metrics.recordEvent(
        Event
          .duration("find_references", timer.elapsed)
          .withLanguage(path.toJLanguage)
          .withOptional("symbol", sym)
          .withLabel("resultCount", resultCount.toString)
      )
      refs
    }
  }

  private def doReferences(
      timer: Timer,
      params: ReferenceParams,
      requestDoc: s.TextDocument,
      enclosingSymbols: Seq[String],
      taskProgress: TaskProgress,
      includeDefinition: Boolean,
      findRealRange: AdjustRange,
      includeSynthetics: Synthetic => Boolean,
  ): List[ReferencesResult] = {
    val path = params.getTextDocument.getUri.toAbsolutePath
    val token = Option(params.getPartialResultToken())
    doReferences(
      timer,
      path,
      token,
      requestDoc,
      enclosingSymbols,
      taskProgress,
      includeDefinition,
      findRealRange,
      includeSynthetics,
    )
  }

  private def doReferences(
      timer: Timer,
      path: AbsolutePath,
      token: Option[JEither[String, Integer]],
      requestDoc: s.TextDocument,
      enclosingSymbols: Seq[String],
      taskProgress: TaskProgress,
      includeDefinition: Boolean,
      findRealRange: AdjustRange,
      includeSynthetics: Synthetic => Boolean,
  ): List[ReferencesResult] = {
    val superMembers = for {
      enclosing <- enclosingSymbols
      info <- requestDoc.symbols.find(_.symbol == enclosing).iterator
      isMethod = info.kind.isMethod
      isTypeMember =
        info.kind.isType && Symbol(info.symbol).owner.isType && !Symbol(
          info.symbol
        ).isToplevel
      if isMethod || isTypeMember
      canBeOverridden =
        !isMethod || (info.displayName != "<init>" && !info.isFinal)
      sym <- Option.when(canBeOverridden)(info.symbol).iterator
      overridden <- sym +: info.overriddenSymbols
      if !ignoredSuperSymbols(overridden)
    } yield overridden
    val implementationMethods =
      (findImplementations(
        timer,
        // Use maximum half of the timeout for finding implementations
        timeout.div(2),
        cache,
        superMembers,
        path,
      ) ++ superMembers).distinct
    val toQuerySymbols =
      if (implementationMethods.nonEmpty) implementationMethods
      else enclosingSymbols

    if (path.isProtoFilename) {
      return protobufReferences.references(
        path,
        timer,
        requestDoc,
        enclosingSymbols,
        toQuerySymbols,
        taskProgress,
        cache.index,
        token,
        includeDefinition,
      )
    }

    val scalaMatchingOccurrence = (for {
      symbol <- toQuerySymbols.iterator
      docAlternatives = SymbolAlternatives.referenceAlternatives(
        symbol,
        requestDoc,
      )
      alternative <- docAlternatives + symbol
    } yield alternative -> symbol).toMap
    val javaMatchingOccurrence = (for {
      symbol <- toQuerySymbols.iterator
      expanded = SymbolAlternatives.expand(Symbol(symbol))
      alternative <- expanded
    } yield alternative -> symbol).toMap
    val enclosingGlobalOccurrences =
      toQuerySymbols.filter(sym => sym.isGlobal)
    val candidatesList =
      if (enclosingGlobalOccurrences.nonEmpty) {
        val candidates = sortedCandidates(
          MbtPossibleReferencesParams(references = toQuerySymbols),
          path,
        )
        // Fallback mode with no build targets
        if (buildTargets.all.isEmpty) candidates
        else {
          val allowedBuildTargets =
            buildTargets
              .inverseSources(path)
              .toList
              .flatMap(
                buildTargets.allInverseDependencies
              )
              .toSet

          candidates.filter { candidate =>
            buildTargets
              .inverseSources(candidate)
              .exists(allowedBuildTargets.contains)
          }
        }
      } else {
        Nil
      }
    val totalCandidates = candidatesList.size
    scribe.info(
      s"references: found $totalCandidates external document candidates in $timer"
    )
    val referenceResults = toQuerySymbols.iterator
      .map(occ => occ -> Buffer.empty[l.Location])
      .toMap
    var processedCandidates = 0

    def addLocation(
        matchSymbol: String,
        symbol: String,
        range: s.Range,
        doc: s.TextDocument,
    ): Unit = {
      val adjustedRange =
        findRealRange(range, doc.text, symbol)
      for {
        realRange <- adjustedRange
        x <- referenceResults.get(matchSymbol)
      } {
        val location = realRange.toLocation(doc.uri)
        token match {
          case Some(token) =>
            languageClient.notifyProgress(
              new l.ProgressParams(token, JEither.forRight(location))
            )
          case None =>
            x += location
        }
      }
    }

    def processDoc(doc: s.TextDocument): Unit = {
      for {
        occ <- doc.occurrences
        if occ.symbol.isGlobal || doc.eq(requestDoc)
        range <- occ.range.toList
        matchSymbol <-
          if (doc.language.isScala) scalaMatchingOccurrence.get(occ.symbol)
          else javaMatchingOccurrence.get(occ.symbol)
        // Exclude definition occurrences for alternate symbols. For example, when
        // doing find-refs on a class symbol, we want usages of the class
        // constructor but not definitions of those constructors.
        if !occ.role.isDefinition || includeDefinition
      } {
        addLocation(matchSymbol, occ.symbol, range, doc)
      }

      for {
        synthetic <- doc.synthetics
        if includeSynthetics(synthetic)
        range <- synthetic.range.toList
        matchSymbol <- {
          val matchingOccurrence =
            if (doc.language.isScala) scalaMatchingOccurrence
            else javaMatchingOccurrence
          matchingOccurrence.iterator.collectFirst {
            case (sym, orig) if Synthetics.existsSymbol(synthetic)(_ == sym) =>
              orig
          }
        }.toList
      } {
        addLocation(matchSymbol, matchSymbol, range, doc)
      }
    }

    // Process the request document first
    processDoc(requestDoc)

    // Process external candidates with progress reporting
    val remaining = groupPathsForIndexing(candidatesList)
    while (remaining.hasNext && !timer.hasElapsed(timeout)) {
      val candidates = remaining.next()
      val docTimer = new Timer(time)
      val docs = cache.index(candidates).documents
      scribe.info(
        s"references: indexed ${candidates.length} candidates in $docTimer"
      )
      docs.foreach(processDoc)
      processedCandidates += candidates.length
      taskProgress.update(processedCandidates, totalCandidates)
    }
    val isIncomplete = remaining.hasNext
    if (isIncomplete) {
      scribe.warn("references timed out, returning partial results")
      taskProgress.update(
        processedCandidates,
        totalCandidates,
        Some("Time out analyzing candidate files"),
      )
    }
    val resultCount = referenceResults.valuesIterator.map(_.size).sum
    scribe.info(
      s"references: found $resultCount reference results in $timer"
    )
    val results = referenceResults.iterator.map { case (symbol, locations) =>
      ReferencesResult(
        symbol,
        locations.toSeq,
        isIncomplete = isIncomplete,
        processedCandidates = processedCandidates,
        totalCandidates = totalCandidates,
      )
    }.toList
    if (isIncomplete && results.isEmpty)
      List(
        ReferencesResult(
          "",
          Nil,
          isIncomplete = true,
          processedCandidates = processedCandidates,
          totalCandidates = totalCandidates,
        )
      )
    else results
  }

  private def enclosingOccurrences(
      requestDoc: s.TextDocument,
      pos: l.Position,
  ): Seq[s.SymbolOccurrence] = {
    val enclosingOccurrencesOriginal = for {
      occ <- requestDoc.occurrences
      range <- occ.range.toList
      if range.encloses(pos)
    } yield occ
    if (enclosingOccurrencesOriginal.exists(_.role.isDefinition))
      // HACK: only show references to the definition *if* there is at least
      // one definition enclosing the range. This prevents unwanted behavior
      // where, for example, a Java enum memeber declares a symbol (the enum
      // member) and also has a synthetic reference to the class constructor
      // (which we don't want to find usages of). Ideally, semanticdb-javac
      // wouldn't emit the synthetic reference so this hack isn't needed.
      enclosingOccurrencesOriginal.filter(_.role.isDefinition)
    else enclosingOccurrencesOriginal
  }

  private def findImplementations(
      timer: Timer,
      timeout: FiniteDuration,
      cache: TextDocumentCache,
      overriddenSymbols: Seq[String],
      path: AbsolutePath,
  ): Seq[String] = {
    val candidates = sortedCandidates(
      MbtPossibleReferencesParams(implementations = overriddenSymbols),
      path,
    )
    if (candidates.isEmpty) {
      return Nil
    }
    val isOverridenSymbol = overriddenSymbols.toSet
    (for {
      paths <- groupPathsForIndexing(candidates)
      if !timer.hasElapsed(timeout)
      doc <- cache.index(paths).documents
      info <- doc.symbols.iterator
      overrideSymbol <- info.overriddenSymbols.iterator
      if isOverridenSymbol(overrideSymbol)
    } yield info.symbol).toSeq
  }

  // NOTE: this cache is similar to InteractiveSemanticdbs and it makes sense to
  // extract it out to a reused class. InteractiveSemanticdbs is used in a lot
  // of places in the codebase and it doesn't support the batch indexing that
  // find-refs needs to get acceptable performance. For time being, it's fine to
  // keep this in-memory cache here. Ideally, when we build the shared
  // abstraction, it will also be powered by a persistent cache, and it's able
  // to deal with adjusting positions in stale payloads, etc.
  // When useFallbackCompiler is true, all files are indexed using the fallback
  // compiler instead of per-build-target compilers. This avoids starting many
  // presentation compilers when MBT is available and provides all sources and
  // dependencies through the fallback compiler. In BSP mode, SemanticDBs are
  // tried first and per-target presentation compilers are used as a fallback.
  private class TextDocumentCache(
      maxCacheSize: () => Int
  ) {
    private final class CompilerContext(
        val compiler: scala.meta.pc.PresentationCompiler,
        val epoch: String,
    ) {
      override def hashCode(): Int =
        31 * System.identityHashCode(compiler) + epoch.hashCode
      override def equals(other: Any): Boolean = other match {
        case context: CompilerContext =>
          (compiler eq context.compiler) && epoch == context.epoch
        case _ => false
      }
    }
    private type CacheKey = (AbsolutePath, Option[CompilerContext], Boolean)
    private def cacheKey(path: AbsolutePath, compilerOnly: Boolean): CacheKey =
      (
        path,
        compilers
          .semanticdbCompilerContext(
            path,
            useFallbackCompiler = !compilerOnly && isMbtMode,
            requireBuildTargetClasspath = compilerOnly,
          )
          .flatMap(compiler =>
            Option(compiler.semanticdbContextEpoch())
              .map(epoch => new CompilerContext(compiler, epoch))
          ),
        compilerOnly,
      )

    private val cache: java.util.Map[CacheKey, s.TextDocument] =
      Collections.synchronizedMap(
        new JLinkedHashMap[CacheKey, s.TextDocument](
          16, // initial capacity
          0.75f, // load factor
          true, // accessOrder - enables LRU ordering
        ) {
          override def removeEldestEntry(
              eldest: java.util.Map.Entry[CacheKey, s.TextDocument]
          ): Boolean = size() > maxCacheSize()
        }
      )

    private def admit(
        key: CacheKey,
        doc: s.TextDocument,
        token: CancelToken,
    ): Unit = {
      token.checkCanceled()
      if (key._2.nonEmpty && key == cacheKey(key._1, key._3)) {
        cache.put(key, doc)
        if (token.isCanceled()) {
          cache.remove(key, doc)
          token.checkCanceled()
        }
      }
    }

    // URI+MD5 disk entries do not carry target/classpath/provider context.
    // Only current BSP SemanticDB or context-keyed in-memory results are admitted.

    def indexSingle(
        path: AbsolutePath,
        token: CancelToken = EmptyCancelToken,
    ): s.TextDocument = {
      indexWithToken(Seq(path), token).documents.headOption.getOrElse {
        scribe.warn(s"references: no document found for $path")
        s.TextDocument()
      }
    }
    def index(paths: Seq[AbsolutePath]): s.TextDocuments =
      index(paths, compilerOnly = false, EmptyCancelToken)

    def indexWithToken(
        paths: Seq[AbsolutePath],
        token: CancelToken,
    ): s.TextDocuments =
      index(paths, compilerOnly = false, token)

    def indexCompilerOnly(
        paths: Seq[AbsolutePath],
        token: CancelToken = EmptyCancelToken,
    ): s.TextDocuments =
      index(paths, compilerOnly = true, token)

    private def index(
        paths: Seq[AbsolutePath],
        compilerOnly: Boolean,
        token: CancelToken,
    ): s.TextDocuments = {
      token.checkCanceled()
      val docs = Buffer.empty[s.TextDocument]
      val contexts =
        paths.map { path =>
          token.checkCanceled()
          path -> cacheKey(path, compilerOnly)
        }.toMap
      val pathsWithMd5 = paths.collect {
        case path if path.exists || buffers.contains(path) =>
          val input = path.toInputFromBuffers(buffers)
          val md5 = MD5.compute(input.text)
          path -> md5
      }.toMap

      def admittedDocuments(): s.TextDocuments = {
        token.checkCanceled()
        val current = docs.filter { doc =>
          doc.uri.toAbsolutePathSafe.exists { path =>
            contexts.get(path).contains(cacheKey(path, compilerOnly)) &&
            pathsWithMd5.get(path).contains(doc.md5) &&
            MD5.compute(path.toInputFromBuffers(buffers).text) == doc.md5
          }
        }
        token.checkCanceled()
        s.TextDocuments(documents = current.toSeq.sortBy(_.uri))
      }
      val toIndex = pathsWithMd5.filter { case (path, md5) =>
        Option(cache.get(contexts(path))) match {
          case Some(doc) if contexts(path)._2.nonEmpty && doc.md5 == md5 =>
            docs += doc
            false
          case _ =>
            true
        }
      }
      if (toIndex.isEmpty) {
        admittedDocuments()
      } else {
        val needCompiler = Buffer.empty[AbsolutePath]
        for ((path, md5) <- toIndex.toSeq.sortBy(_._1.toString)) {
          val bspDocument =
            if (compilerOnly) None
            else semanticdbs().textDocument(path).toOption
          bspDocument match {
            case Some(doc) if doc.md5 == md5 =>
              // basic semanticdb uses relative uris
              val docAdjusted = doc.withUri(path.toURI.toString)
              docs += docAdjusted
              admit(contexts(path), docAdjusted, token)
            case _ =>
              needCompiler += path
          }
        }
        if (needCompiler.isEmpty) {
          admittedDocuments()
        } else {
          val result = batchCompileTextDocuments(
            needCompiler.toSeq,
            useFallbackCompiler = !compilerOnly && isMbtMode,
            requireBuildTargetClasspath = compilerOnly,
            token = token,
          )
          for {
            doc <- result
            path <- doc.uri.toAbsolutePathSafe
            expectedMd5 <- pathsWithMd5.get(path)
            if contexts(path) == cacheKey(path, compilerOnly)
            if doc.md5 == expectedMd5
            if MD5.compute(path.toInputFromBuffers(buffers).text) == expectedMd5
          } {
            admit(contexts(path), doc, token)
          }
          token.checkCanceled()
          docs ++= result.filter { doc =>
            doc.uri.toAbsolutePathSafe.exists { path =>
              contexts(path) == cacheKey(path, compilerOnly) &&
              pathsWithMd5.get(path).contains(doc.md5) &&
              MD5.compute(path.toInputFromBuffers(buffers).text) == doc.md5
            }
          }
          admittedDocuments()
        }
      }
    }
  }

}

case class MbtClassImplementation(
    symbol: String,
    path: AbsolutePath,
    range: l.Range,
    info: Option[s.SymbolInformation],
)
