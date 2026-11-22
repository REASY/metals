package scala.meta.internal.metals

import java.lang
import java.net.URI
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.util.Optional
import java.util.function.Supplier
import java.util.logging.Logger

import scala.util.Try
import scala.util.control.NonFatal
import scala.util.matching.Regex

import scala.meta.internal.metals.utils.LimitedFilesManager
import scala.meta.internal.metals.utils.TimestampedFile
import scala.meta.internal.mtags.CommonMtagsEnrichments._
import scala.meta.internal.mtags.EncoderDecoder
import scala.meta.pc.{reports => jreports}

trait ReportContext extends jreports.ReportContext {
  def unsanitized: Reporter
  def incognito: Reporter
  def bloop: Reporter
  def all: List[Reporter] = List(unsanitized, incognito, bloop)
  def allToZip: List[Reporter] = List(incognito, bloop)
  def cleanUpOldReports(
      maxReportsNumber: Int = StdReportContext.MAX_NUMBER_OF_REPORTS
  ): Unit = all.foreach(_.cleanUpOldReports(maxReportsNumber))
  def deleteAll(): Unit = all.foreach(_.deleteAll())
}

trait Reporter extends jreports.Reporter {
  def name: String
  def cleanUpOldReports(
      maxReportsNumber: Int = StdReportContext.MAX_NUMBER_OF_REPORTS
  ): List[TimestampedFile]
  def getReports(): List[TimestampedFile]
  def deleteAll(): Unit
  def sanitize(message: String) = message
}

class StdReportContext(
    workspace: Path,
    resolveBuildTarget: Option[URI] => Option[String],
    level: ReportLevel = ReportLevel.Info,
    reportTrackers: List[ReportTracker] = Nil
) extends ReportContext {
  val reportsDir: Path = workspace.resolve(StdReportContext.reportsDir)

  val unsanitized: StdReporter =
    new StdReporter(
      workspace,
      StdReportContext.reportsDir,
      resolveBuildTarget,
      level,
      "metals-full",
      reportTrackers
    )
  val incognito: StdReporter =
    new StdReporter(
      workspace,
      StdReportContext.reportsDir,
      resolveBuildTarget,
      level,
      "metals",
      reportTrackers
    )
  val bloop: StdReporter =
    new StdReporter(
      workspace,
      StdReportContext.reportsDir,
      resolveBuildTarget,
      level,
      "bloop",
      reportTrackers
    )

  override def cleanUpOldReports(
      maxReportsNumber: Int = StdReportContext.MAX_NUMBER_OF_REPORTS
  ): Unit = {
    all.foreach(_.cleanUpOldReports(maxReportsNumber))
  }

  override def deleteAll(): Unit = {
    all.foreach(_.deleteAll())
    val zipFile = reportsDir.resolve(StdReportContext.ZIP_FILE_NAME)
    if (Files.exists(zipFile)) Files.delete(zipFile)
  }
}

class StdReporter(
    workspace: Path,
    pathToReports: Path,
    resolveBuildTarget: Option[URI] => Option[String],
    level: ReportLevel,
    override val name: String,
    reportTrackers: List[ReportTracker]
) extends Reporter {

  private val logger = Logger.getLogger(classOf[ReportContext].getName)
  val maybeReportsDir: Path =
    workspace.resolve(pathToReports).resolve(name)
  private val retention = ReportFileRetention.forDirectory(maybeReportsDir)
  private val limitedFilesManager =
    new LimitedFilesManager(
      maybeReportsDir,
      StdReportContext.MAX_NUMBER_OF_REPORTS,
      ReportFileName.pattern,
      ".md"
    )

  private lazy val userHome = Option(System.getProperty("user.home"))

  def readInIds(): Unit = retention.initialize(limitedFilesManager)

  override def create(
      lazyReport: Supplier[jreports.Report],
      ifVerbose: lang.Boolean
  ): Optional[Path] =
    if (ifVerbose && !level.isVerbose) Optional.empty()
    else
      Try {
        readInIds()
        val report = lazyReport.get()
        val sanitizedId = report.id().asScala.map(sanitize)
        val (pathToReport, created) =
          retention.create(sanitizedId, reportPath(report, _))(
            sanitize(report.fullText( /* withIdAndSummary = */ true))
          )
        if (created) {
          logger.info(s"Created report: ${report.path()}")
          reportTrackers.foreach(_.reportCreated(report))
        }
        if (!ifVerbose)
          logger.info(
            s"${report.shortSummary()} (full report at: ${pathToReport})"
          )
        pathToReport
      }.recoverWith { case NonFatal(error) =>
        logger.warning(
          s"Unable to retain report: ${error.getClass.getSimpleName}"
        )
        scala.util.Failure(error)
      }.toOption
        .asJava

  override def sanitize(text: String): String = {
    val textAfterWokspaceReplace =
      text.replace(workspace.toString(), StdReportContext.WORKSPACE_STR)
    userHome
      .map(textAfterWokspaceReplace.replace(_, StdReportContext.HOME_STR))
      .getOrElse(textAfterWokspaceReplace)
  }

  private def reportPath(report: jreports.Report, timestamp: Long): Path = {
    val instant = Instant.ofEpochMilli(timestamp).atZone(ZoneId.systemDefault())
    val date = DateTimeFormatter.ofPattern("yyyy-MM-dd").format(instant)
    val time = DateTimeFormatter.ofPattern("HH-mm-ss-SSS").format(instant)
    val buildTargetPart =
      resolveBuildTarget(report.path().asScala)
        .map(FileNameEncoderDecoder.encode)
        .map("_(" ++ _ ++ ")")
        .getOrElse("")
    val filename = s"r_${report.name()}${buildTargetPart}_${time}.md"
    require(
      !filename.contains('/') && !filename.contains('\\'),
      "report name must be one filename"
    )
    maybeReportsDir.toAbsolutePath.normalize().resolve(date).resolve(filename)
  }

  override def cleanUpOldReports(
      maxReportsNumber: Int = StdReportContext.MAX_NUMBER_OF_REPORTS
  ): List[TimestampedFile] = {
    readInIds()
    retention.cleanup(maxReportsNumber)
  }

  override def getReports(): List[TimestampedFile] = {
    readInIds()
    retention.reports
  }

  override def deleteAll(): Unit = {
    readInIds()
    retention.deleteAll()
    limitedFilesManager.directoriesWithDate.foreach { d =>
      try Files.delete(d.toPath)
      catch {
        case NonFatal(_) =>
          logger.warning(s"Unable to delete directory $d")
      }
    }
  }

}

object StdReportContext {
  val MAX_NUMBER_OF_REPORTS = 30
  // Per reporter: at most 7.5 MiB of report payloads, plus bounded metadata.
  val MAX_REPORT_BYTES: Int = 256 * 1024
  val WORKSPACE_STR = "<WORKSPACE>"
  val HOME_STR = "<HOME>"
  val ZIP_FILE_NAME = "reports.zip"

  def reportsDir: Path = Paths.get(".metals").resolve(".reports")
}

object EmptyReporter extends Reporter {

  override def cleanUpOldReports(maxReportsNumber: Int): List[TimestampedFile] =
    Nil

  override def create(
      report: Supplier[jreports.Report],
      ifVerbose: lang.Boolean
  ): Optional[Path] =
    Optional.empty()

  override def name = "empty-reporter"

  override def getReports(): List[TimestampedFile] = List()

  override def deleteAll(): Unit = {}
}

object EmptyReportContext extends ReportContext {

  override def unsanitized: Reporter = EmptyReporter

  override def incognito: Reporter = EmptyReporter

  override def bloop: Reporter = EmptyReporter
}

trait ReportTracker {
  def reportCreated(report: jreports.Report): Unit
}

object ReportFileName {
  val pattern: Regex = "r_(?<name>[^()]*)(_\\((?<buildTarget>.*)\\))?_".r

  def getReportNameAndBuildTarget(
      file: TimestampedFile
  ): (String, Option[String]) =
    pattern.findPrefixMatchOf(file.name) match {
      case None => (file.name, None)
      case Some(foundMatch) =>
        (
          foundMatch.group("name"),
          Option(foundMatch.group("buildTarget"))
            .map(FileNameEncoderDecoder.decode)
        )
    }

}

object FileNameEncoderDecoder
    extends EncoderDecoder(
      '_',
      Set('/', '\\', ':', '*', '?', '"', '<', '>', '|', '_', '@')
    )
