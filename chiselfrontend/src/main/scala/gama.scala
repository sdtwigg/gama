package object gama {
  case class UserspaceInfo(file: String, line: String)
  
  case class GamaNote(uinfo: Option[UserspaceInfo])
  object GamaNote {def apply(): GamaNote = GamaNote(None)}
  
  case class Sink[+D](data: D) extends AnyVal
  case class Source[+D](data: D) extends AnyVal
  
  case class Left[+D](data: D) extends AnyVal
  case class Right[+D](data: D) extends AnyVal
}

