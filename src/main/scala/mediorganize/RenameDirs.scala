package mediorganize

import ammonite.ops._
import ammonite.ops.ImplicitWd._
import scala.language.postfixOps

object RenameDirs {
  val listing = ls.rec!
  val dirs = listing.filter(dir => !dir.isFile)
  val template = """^(\d\d\d\d)_(\d\d)_(\d\d)(.*)$""".r
  val target = """^(\d\d\d\d)-(\d\d)-(\d\d)(.*)$""".r

  val shellFile = pwd/"mv.sh"
  write.over(shellFile, "")

  dirs.foreach {
    dir =>
      dir.last match {
        case template(year, month, day, rest) =>
          // rename
          val newName = s"$year-$month-$day${rest.replace("-", "\\ ")}"
          val parent = (dir/up) relativeTo pwd
          val cmd = s"mv ${dir relativeTo pwd} $parent/$newName\n"
          print(cmd)
          write.append(shellFile, cmd)
        case target(year, month, day, rest) =>
        // check that year/month/day matches earliest
        case _ =>
        // do nothing
      }
  }
}
