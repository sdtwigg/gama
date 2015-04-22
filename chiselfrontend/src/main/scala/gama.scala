package object gama {
  case class EnclosureInfo(em: EnclosingModule, debug: Option[UserspaceInfo])
  case class UserspaceInfo(file: String, line: String)
}
