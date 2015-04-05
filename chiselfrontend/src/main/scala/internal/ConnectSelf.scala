package gama
package internal

@annotation.implicitNotFound("Cannot connect elements of type ${D}. No implicit ConnectSelf[${D}] available")
trait ConnectSelf[D<:Data] {
  def connectSelf(source: D, sink: D, em: EnclosingModule): D
}
object ConnectSelf {
  def apply[D<:Data: ConnectSelf] = implicitly[ConnectSelf[D]]
  trait ConnectSelfImpl[D<:Data] extends ConnectSelf[D] {
    def verifyConnectSelf(source: D, sink: D): Unit
    def connectSelf(source: D, sink: D, em: EnclosingModule): D = {
      em.getActiveJournal.append(ConnectData(source, sink))
      sink
    }
  }
}
