package object mediorganize {
  import java.security.{DigestInputStream, MessageDigest}
  import java.text.SimpleDateFormat
  import java.util.Calendar

  import ammonite.ops.ImplicitWd._
  import ammonite.ops._

  import scala.language.postfixOps
  import scala.util.matching.Regex

  // extensions are treated as case-insensitive
  val ImageExtensions: Set[String] = {
    val lowercase = Set("jpg", "jpeg", "gif", "png", "tif", "tiff", "bmp", "cr2", "ppm", "crw", "dcm")
    lowercase ++ lowercase.map(_.toUpperCase)
  }
  val VideoExtensions: Set[String] = {
    val lowercase = Set("mov", "m4v", "thm", "mp4", "mod", "mpeg", "mpg", "mp2",  "avi")
    lowercase ++ lowercase.map(_.toUpperCase)
  }
  val AllExtensions                  : Set[String] = ImageExtensions ++ VideoExtensions
  val UnsupportedExtensionsForWriting: Set[String] = {
    val lowercase = Set("mod", "avi", "mpg")
    lowercase ++ lowercase.map(_.toUpperCase)
  }

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
  val AllPhotoDateFields : Seq[String] = Seq(DateTimeOriginal, CreateDate, ModifyDate)
  val AllVideoDateFields : Seq[String] = Seq(DateTimeOriginal, FileModifyDate, CreateDate, ModifyDate, TrackCreateDate, TrackModifyDate, MediaCreateDate, MediaModifyDate)

  val ExifFieldRegex  : Regex = """^([a-zA-Z0-9_\-]+) +: +(.*)$""".r
  val ExifYearMonthDay: Regex = """^(\d\d\d\d):(\d\d):(\d\d).*$""".r
  val ExifFullDate    : Regex = """^(\d{4})(?::(\d\d)(?::(\d\d)(?: (\d\d)(?::(\d\d)(?::(\d\d)(?:([+\-]\d\d:\d\d))?)?)?)?)?)?""".r
  val FileDateRegex   : Regex = """(\d{4}\b)(?:[_\-](\d\d)\b(?:[_\-](\d\d)\b(?:[_\-](\d\d)\b(?:[_\-](\d\d)\b(?:[_\-](\d\d)\b)?)?)?)?)?.*""".r // month and day are optional

  def recursiveFileListing(root: Path, extensions: Set[String]): Seq[Path] = {
    require(root.isDir, s"directory required for recursive listing. Was: $root")
    val listing = ls.rec! root
    listing.filter(file => file.isFile && extensions.contains(file.ext))
  }

  def isImage(file: Path): Boolean = {
    require(file.isFile, s"target must be a file. was: $file")
    ImageExtensions.contains(file.ext)
  }

  def isFile(file: Path): Boolean = {
    require(file.isFile, s"target must be a file. was: $file")
    VideoExtensions.contains(file.ext)
  }

  case class FileDate(year    : Option[String],
                      month   : Option[String],
                      day     : Option[String],
                      hour    : Option[String],
                      minute  : Option[String],
                      second  : Option[String],
                      timezone: Option[String]) {
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

  object FileDate {
    val Empty: FileDate = apply()

    def apply(year: String = null,
              month: String = null,
              day: String = null,
              hour: String = null,
              minute: String = null,
              second: String = null,
              timezone: String = null): FileDate = {
      FileDate(Option(year), Option(month), Option(day), Option(hour),
        Option(minute), Option(second), Option(timezone))
    }

    // assumes EXIF format, YYYY:mm:dd HH:MM:SS
    def fromExif(date: String): Option[FileDate] = {
      date match {
        case ExifFullDate(year, month, day, hour, minute, second, timezone) =>
          Some(apply(year, month, day, hour, minute, second, timezone))
        case _ =>
          None
      }
    }

    def fromPath(file: Path): Option[FileDate] = {
      require(file.isFile)
      file.last match {
        case FileDateRegex(year, month, day, hour, minute, second) =>
          Some(apply(year, month, day, hour, minute, second))
        case _ =>
          (file / up).last match {
            case FileDateRegex(year, month, day, hour, minute, second) =>
              Some(apply(year, month, day, hour, minute, second))
            case _ =>
              None
          }
      }

    }
  }

  case class MetadataReport(
                             path          : Path,
                             preferredDate : FileDate,
                             exifDateFields: Seq[(String, FileDate)]) {
    val (suggestedDate, changesNeeded): (Option[FileDate], Boolean) = {
      // TODO: if date day from exif DateTimeOriginal is after the day of the file date, then use the DateTimeOriginal. year and month must match.

      val fieldMap = exifDateFields.toMap

      // ignore FileModifyDate if the file is an image
      // ignore the timezone when looking for unique entries
      val uniqueDates = {
        // ignore modified dates for images
        if (isImage(path)) exifDateFields.filterNot(f => f._1 == FileModifyDate || f._1 == ModifyDate) else exifDateFields
      }.map(_._2.withoutTimezone).toSet

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
          // see if month and year matches DateTimeOriginal
          if (fieldMap.contains(DateTimeOriginal) &&
              fieldMap(DateTimeOriginal).year == preferredDate.year &&
              fieldMap(DateTimeOriginal).month == preferredDate.month) {
            (Some(fieldMap(DateTimeOriginal)), uniqueDates.size > 1)
          } else {
            // use the preferred date from the filename as a replacement
            (Some(preferredDate.full), true)
          }
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

  def exifDates(file: Path): Seq[(String, FileDate)] = {
    require(file.isFile)
    val options = AllDateFields.map("-" + _)
    val cmd     = %%.extend("exiftool" +: "-s" +: options, Map.empty)
    try {
      cmd(file).out.lines.flatMap {
        case ExifFieldRegex(key, value) =>
          FileDate.fromExif(value).map(v => key -> v)
        case _ =>
          None
      }
    } catch {
      // TODO: use Try instead
      case ShelloutException(res) =>
        println(s"error with file path $file")
        println(res.err.string)
        Seq.empty
    }
  }


  def exif(file: Path): Seq[(String, String)] = {
    require(file.isFile)
    try {
      val result = %%exiftool("-s", file)
      result.out.lines.flatMap {
        case ExifFieldRegex(key, value) =>
          Some(key -> value)
        case _ =>
          None
      }
    } catch {
      // TODO: use Try instead
      case ShelloutException(res) =>
        println(s"error obtaining exif from file $file")
        println(res.err.string)
        println(res.out.string)
        Seq.empty
    }
  }

  def setExifDates(file: Path, fields: Seq[(String, FileDate)]): Unit = {
    require(file.isFile)
    val options = fields.map{case(key, date) => s"-$key=$date"}
    val cmd     = %.extend("exiftool" +: options, Map.empty)
    println(s"$file --> ${fields.map{case(k,v) => s"$k=$v"}.mkString("  ")}")
    try {
      cmd(file)
    } catch {
      case ShelloutException(res) =>
        println(s"error updating file $file")
        println(res.err.string)
    }
  }

  def report(file: Path): MetadataReport = {
    require(file.isFile)
    val dateFromFile = FileDate.fromPath(file).getOrElse(FileDate.Empty)
    val datesFromExif = exifDates(file)
    MetadataReport(file, dateFromFile, datesFromExif)
  }

  def now(): String = {
    val dateFormat  = new SimpleDateFormat("YYYY-MM-DD-HH-mm-ss")
    dateFormat.format(Calendar.getInstance.getTime)
  }


  def crawl(dir: Path, extensions: Set[String] = AllExtensions): Unit = {
    require(dir.isDir, s"crawl target must be a directory. was: $dir")
    // TODO: keep track of what's already been recorded so the job can be restarted

    val dateStr     = now()
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

  case class MediaInfo(
                      relPath: RelPath,
                      sha1: String,
                      size: Long,
                      exif: Map[String, String]
                      )

  def mediaInfo(file: Path, base: Path = pwd): MediaInfo = {
    require(file.isFile)
    val rel = file.relativeTo(base)
    val sha = toHex(sha1(file))
    val stats = stat! file
    val exifFields = exif(file)
    MediaInfo(rel, sha, stats.size, exifFields.toMap)
  }

  def index(dir: Path, extensions: Set[String] = AllExtensions): Unit = {
    val index   = dir/s"${now()} index.json"
    recursiveFileListing(dir, extensions).foreach {
      file =>
        val info = mediaInfo(file, dir)
        // each line in the index is an independent JSON object to be read
        write.append(index, upickle.default.write(info) + "\n")
    }
  }

  def readIndex(idx: Path): Seq[MediaInfo] = {
    require(idx.isFile)
    read(idx).lines.map {
      line =>
        upickle.default.read[MediaInfo](line)
    }.toSeq
  }

  // BEWARE: uses blocking I/O
  def sha1(file: Path): Array[Byte] = {
    require(file.isFile)
    val hasher = MessageDigest.getInstance("SHA-1")
    val stream = read.getInputStream(file)
    val digestStream = new DigestInputStream(stream, hasher)
    val buffer = new Array[Byte](1024) // TODO: configure buffer size? use available() to reduce blocking?
    while(digestStream.read(buffer) > -1) {
      // keep on reading
    }
    digestStream.getMessageDigest.digest()
  }

  def toHex(bytes: Array[Byte]): String = {
    bytes.map("%02x" format _).mkString
  }

  def correct(corrections: Path): Unit = {
    require(corrections.isFile, s"corrections must be a file. was: $corrections")
    val dateStr = now()
    val fixed   = corrections/up/s"$dateStr fixed.csv"

    read.lines(corrections).foreach {
      line =>
        val fields        = line.split(",")
        val shouldCorrect = fields(4).toBoolean
        if (shouldCorrect) {
          val path        = Path(fields(1))
          // can't write some file types
          if (!UnsupportedExtensionsForWriting.contains(path.ext)) {
            val targetDate  = FileDate.fromExif(fields(5)).get
            val fieldsToSet = if (isImage(path)) AllPhotoDateFields else AllVideoDateFields
            val changes     = fieldsToSet.map(_ -> targetDate)
            setExifDates(path, changes)
            write.append(fixed, s"$path,$targetDate,${fieldsToSet.mkString(",")}\n")
          }
        }
    }
  }
}
