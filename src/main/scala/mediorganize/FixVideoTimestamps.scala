package mediorganize

import ammonite.ops._

object FixVideoTimestamps extends App {
  val listing = ls.rec!
  val movieExts = List("MOV", "m4v", "THM", "MP4", "mov", "mp4", "mpg")
  val videos = listing.filter(file => file.isFile && movieExts.contains(file.ext))

  val altRegex = """^FileModifyDate +: +(.*)$""".r
  val exifDate = """^(\d\d\d\d):(\d\d):(\d\d).*$""".r
  val dater = """^(\d\d\d\d)_(\d\d)_(\d\d).*$""".r
  val dater2 = """^(\d\d\d\d)-(\d\d)-(\d\d).*$""".r

  def getDate(file: Path): Option[String] = {
    // TODO: look at other fields for file date (matching y/m/d)
    val result = %%exiftool("-s", "-FileModifyDate", file)

    result.head match {
      case altRegex(altDate) => Some(altDate)
      case _ => None
    }
  }

  def parseExifDate(str: String): Option[(String, String, String)] = {
    str match {
      case exifDate(year, month, day) => Some((year, month, day))
      case _ => None
    }
  }

  def optToString[T](opt: Option[T]): String = {
    opt match {
      case None => ""
      case Some(t) => t.toString
    }
  }

  def changeDate(date: String, file: Path): String = {
    val options = List("FileModifyDate", "CreateDate", "ModifyDate", "TrackCreateDate", "TrackModifyDate", "MediaCreateDate",
      "MediaModifyDate", "AllDates").map(tag => s""""-$tag=$date"""").mkString(" ")
    val relPath = file relativeTo cwd
    val relPathStr = relPath.toString.replace(" ", "\\ ")
    s"""echo "$date -> $relPath"\nexiftool $options $relPathStr\n"""
  }

  val shellFile = cwd/"commands.sh"
  write.over(shellFile, "")
  videos.foreach {
    video =>
      val dirname = (video / up).last

      val maybeDirDate = dirname match {
        case dater(year, month, day) => Some(s"$year:$month:$day 00:00:00-08:00")
        case dater2(year, month, day) => Some(s"$year:$month:$day 00:00:00-08:00")
        case _ => None
      }

      val altFilename = video.last.substring(0, video.last.indexOf(".")) + ".MOD"
      val altFile = ls! video / up |? (_.last == altFilename)
      val maybeDate =
        if (altFile.nonEmpty) getDate(altFile.head)
        else maybeDirDate

      maybeDate.foreach {
        date =>
          val maybeCurDate = getDate(video)
          val dateNeedsAdjusting = maybeCurDate match {
            case None => true
            case Some(currentDate) =>
              if (parseExifDate(currentDate) == parseExifDate(date)) false
              else true
          }
          if (dateNeedsAdjusting) {
            println(s"${optToString(maybeCurDate)} -> $date | $video")
            write.append(shellFile, changeDate(date, video))
          }
      }
  }
}
