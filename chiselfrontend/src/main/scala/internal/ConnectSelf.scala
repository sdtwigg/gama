package gama
package internal

case class Sink[+D<:Data](data: D) extends AnyVal
case class Source[+D<:Data](data: D) extends AnyVal

@annotation.implicitNotFound("Cannot connect elements of type ${D}. No implicit ConnectSelf[${D}] available")
trait ConnectSelf[D<:Data] {
  def connectSelf(sink: Sink[D], source: Source[D], em: EnclosingModule): D
}
object ConnectSelf {
  def apply[D<:Data: ConnectSelf] = implicitly[ConnectSelf[D]]
  trait ConnectSelfImpl[D<:Data] extends ConnectSelf[D] {
    def verifyConnectSelf(sink: Sink[D], source: Source[D]): Unit
    def connectSelf(sink: Sink[D], source: Source[D], em: EnclosingModule): D = {
      em.getActiveJournal.append(ConnectData(sink, source))
      sink.data
    }
  }
}

