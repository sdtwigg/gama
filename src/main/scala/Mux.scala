package gama

trait SelfMuxable[D[X<:Node[Y],Y<:NS_UB] <: Data[X,Y], NS_UB <: NodeStore] {
  def mux(left: D[_<:Node[_<:NS_UB],_<:NS_UB], right: D[_<:Node[_<:NS_UB],_<:NS_UB]): D[Mux[NS_UB],NS_UB]
}
/*
trait Muxable[L[X<:Node[Y],Y<:LNS_UB] <: Data[X,Y],-LNS_UB <: NodeStore,
              R[X<:Node[Y],Y<:RNS_UB] <: Data[X,Y],-RNS_UB <: NodeStore]
{
  def mux[T[X<:Node[Y],Y<:NS]<:Data[X,Y],NS:>LNS_UB:>RNS_UB](
         left: T[_<:Node[NS],NS] with L[_<:Node[_<:LNS_UB],_<:LNS_UB],
        right: T[_<:Node[NS],NS] with R[_<:Node[_<:RNS_UB],_<:RNS_UB]): T[Mux[NS],NS]
}
*/

class Mux[+NS <: NodeStore](storage: NS) extends Op(storage)

object Mux {
  def makeMux[D[X<:Node[Y],Y<:NS]<:Data[X,Y],NS<:NodeStore](
    left: D[_<:Node[NS],NS], right: D[_<:Node[NS],NS])(implicit wizard: SelfMuxable[D,NS]) = {
      wizard.mux(left, right)
  }
}
