package mary

sealed trait FileTypes

object FileTypes {
  case object All extends FileTypes
  case object Dir extends FileTypes
  case object File extends FileTypes
}
