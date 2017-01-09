import ammonite.ops._
import ammonite.ops.ImplicitWd._
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.language.postfixOps
import scala.util.matching.Regex

package object mediorganize {
  // extensions are treated as case-insensitive
  val ImageExtensions: Set[String] = Set("jpg", "jpeg", "gif", "png", "tif", "tiff", "bmp", "cr2", "ppm", "crw", "dcm")
  val VideoExtensions: Set[String] = Set("mov", "m4v", "thm", "mp4", "mod", "mpeg", "mpg", "mp2",  "avi")
  val AllExtensions  : Set[String] = ImageExtensions ++ VideoExtensions

  val DateTimeOriginal = "DateTimeOriginal"
  val FileModifyDate   = "FileModifyDate"
  val CreateDate       = "CreateDate"
  val ModifyDate       = "ModifyDate"
  val TrackCreateDate  = "TrackCreateDate"
  val TrackModifyDate  = "TrackModifyDate"
  val MediaCreateDate  = "MediaCreateDate"
  val MediaModifyDate  = "MediaModifyDate"
  val AllDates         = "AllDates"

  val AllDateFields      : Seq[String] = Seq(FileModifyDate, TrackCreateDate, TrackModifyDate, MediaCreateDate, MediaModifyDate, AllDates)
  val DateFieldPrecedence: Seq[String] = Seq(DateTimeOriginal, CreateDate, TrackCreateDate)

  val ExifFieldRegex  : Regex = """^([a-zA-Z0-9_]+) +: +(.*)$""".r
  val ExifYearMonthDay: Regex = """^(\d\d\d\d):(\d\d):(\d\d).*$""".r
  val ExifFullDate    : Regex = """^(\d{4})(?::(\d\d)(?::(\d\d)(?: (\d\d)(?::(\d\d)(?::(\d\d)(?:([+\-]\d\d:\d\d))?)?)?)?)?)?""".r
  val FileYearMonthDay: Regex = """(\d{4})(?:-(\d\d)(?:-(\d\d))?)?.*""".r // month and day are optional

  def recursiveFileListing(root: Path, extensions: Set[String]): Seq[Path] = {
    require(root.isDir, s"directory required for recursive listing. Was: $root")
    val listing = ls.rec! root
    listing.filter(file => file.isFile && extensions.contains(file.ext))
  }

  case class FileDate(year    : Option[String],
                      month   : Option[String],
                      day     : Option[String],
                      hour    : Option[String] = None,
                      minute  : Option[String] = None,
                      second  : Option[String] = None,
                      timezone: Option[String] = None) {
    val optionalSeq: Seq[Option[String]] = {
      Seq(year, month, day, hour, minute, second)
    }

    {
      val nones = optionalSeq.dropWhile(_.nonEmpty)
      require(nones.forall(_.isEmpty), s"file date must be prefix only. was: $this")
    }

    val toSeq  : Seq[String] = optionalSeq.flatten
    val size   : Int      = toSeq.size
    val isEmpty: Boolean  = toSeq.isEmpty
    val isFull : Boolean  = this.size == 7

    def full: FileDate = {
      require(!this.isEmpty)
      FileDate(
        year,
        month   .orElse(Some("01")),
        day     .orElse(Some("01")),
        hour    .orElse(Some("00")),
        minute  .orElse(Some("00")),
        second  .orElse(Some("00")),
        timezone.orElse(Some("-08:00")))
    }

    def withoutTimezone: FileDate = FileDate(year, month, day, hour, minute, second, None)

    def isPrefixOf(other: FileDate): Boolean = {
      this.size <= other.size && other.toSeq.take(this.size) == this.toSeq
    }

    private[this] def withPrefix(prefix: String, field: Option[String]): String = {
      if (field.isEmpty) "" else prefix + field.get
    }
    override def toString: String = {
      s"${year.getOrElse("")}${withPrefix(":", month)}${withPrefix(":", day)}" +
        s"${withPrefix(" ", hour)}${withPrefix(":", minute)}${withPrefix(":", second)}" +
        s"${withPrefix("", timezone)}"
    }
  }

  case class MetadataReport(
                             path          : Path,
                             preferredDate : FileDate,
                             exifDateFields: Seq[(String, FileDate)]) {
    val (suggestedDate, changesNeeded): (Option[FileDate], Boolean) = {
      val fieldMap = exifDateFields.toMap
      val uniqueDates = exifDateFields.map(_._2.withoutTimezone).toSet // ignore the timezone
      val prefixMatches = uniqueDates.filter(preferredDate.isPrefixOf)
      if (prefixMatches.size == 1) {
        if (uniqueDates.size == 1) {
          // there is one date in the exif and it matches the name preferred (if any). Do nothing.
          (None, false)
        } else {
          // there's more than one date in the exif, so need to overwrite with the preferred date
          (Some(prefixMatches.head), true)
        }
      } else if (prefixMatches.isEmpty) {
        // none of the dates matches the preferred date
        if (preferredDate.isEmpty) {
          // we don't have any information about the preferred date so we can't suggest changes
          (None, true)
        } else {
          // use the preferred date from the filename as a replacement
          (Some(preferredDate.full), true)
        }
      } else {
        // there is more than one prefix match. Need to choose one.
        // look at fields in order of precedence and use the first one
        (DateFieldPrecedence.collectFirst {
          case field if fieldMap.contains(field) && prefixMatches.contains(fieldMap(field).withoutTimezone) =>
            fieldMap(field)
        }, true)
      }
    }

    def toCsv: String = {
      val url = s"file://$path"
      val fixedFields = Seq(path.last, path, url, preferredDate, changesNeeded, suggestedDate.map(_.toString).getOrElse("")).map(_.toString)
      (fixedFields ++ exifDateFields.map{case(key, value) => s"$key=$value"}).mkString(",")
    }
  }

  def fileDate(file: Path): FileDate = {
    require(file.isFile)
    val (maybeYear, maybeMonth, maybeDay) = file.last match {
      case FileYearMonthDay(year, month, day) =>
        (Some(year), Option(month), Option(day))
      case _ =>
        (file / up).last match {
          case FileYearMonthDay(year, month, day) =>
            (Some(year), Option(month), Option(day))
          case _ =>
            (None, None, None)
        }
    }
    new FileDate(maybeYear, maybeMonth, maybeDay)
  }

  def exifDates(file: Path): Seq[(String, FileDate)] = {
    require(file.isFile)
    val options = AllDateFields.map("-" + _)
    val cmd = %%.extend("exiftool" +: "-s" +: options, Map.empty)
    val result = cmd(file)
    result.out.lines.flatMap {
      case ExifFieldRegex(key, value) =>
        value match {
          case ExifFullDate(year, month, day, hour, minute, second, timezone) =>
            val date = FileDate(
              Option(year),
              Option(month),
              Option(day),
              Option(hour),
              Option(minute),
              Option(second),
              Option(timezone))
            Some(key -> date)
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  def report(file: Path): MetadataReport = {
    require(file.isFile)
    val dateFromFile = fileDate(file)
    val datesFromExif = exifDates(file)
    MetadataReport(file, dateFromFile, datesFromExif)
  }


  def crawl(dir: Path, extensions: Set[String] = AllExtensions): Unit = {
    require(dir.isDir, s"crawl target must be a directory. was: $dir")
    // TODO: keep track of what's already been recorded so the job can be restarted

    val dateFormat  = new SimpleDateFormat("YYYY-MM-DD HH:mm:ss")
    val dateStr     = dateFormat.format(Calendar.getInstance.getTime)
    val corrections = dir/s"$dateStr corrections.csv"
    val all         = dir/s"$dateStr all.csv"

    recursiveFileListing(dir, extensions).foreach {
      file =>
        val rep = report(file)
        if (rep.changesNeeded) {
          write.append(corrections, rep.toCsv + "\n")
        }
        write.append(all, rep.toCsv + "\n")
    }

  }
}
