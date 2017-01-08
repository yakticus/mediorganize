package mediorganize

// features:
// dup detection -- save hash of image contents
// handle all file types (photos & videos)
// EXIF consistency check:
//    - all dates must be the same
//    - date must match file/directory name as much as possible: YYYY[-MM[-DD[-HH[-MM]]]]
//        - date pattern in filename takes precedence over date pattern in directory
// EXIF consistency report:
//   - must be actionable (i.e., script can take results and "make it so")
//   - stretch: contain visual (thumbnail)
//   - contain suggested "edit" (i.e., date to replace all dates)
//   - suggestion can be manually overridden before acting upon it

object TestMain extends App {
  println("what did the world say to itself? Hello!")
}
