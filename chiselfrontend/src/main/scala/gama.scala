package object gama {
  case class UserspaceInfo(file: String, line: String)
  
  sealed trait GamaTreeSource
  case class GTSourceUserspace(uinfo: UserspaceInfo) extends GamaTreeSource
  case class GTSourcePass(passid: String) extends GamaTreeSource
  case object GTSourceUNKNOWN extends GamaTreeSource
 
  case class GamaNote(source: GamaTreeSource)
  object GamaNote {
    def unknown: GamaNote = GamaNote(GTSourceUNKNOWN)
  }
  
  case class Sink[+D](data: D) extends AnyVal
  case class Source[+D](data: D) extends AnyVal
  
  case class Left[+D](data: D) extends AnyVal
  case class Right[+D](data: D) extends AnyVal
}

