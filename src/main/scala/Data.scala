package gama

class Data[+N<:Node[NS], +NS<:NodeStore]

trait Regenerate[D[X<:Node[Y],Y<:NSrestrict] <: Data[X,Y], -NSrestrict <: NodeStore] {
  def regenerate[NS<:NSrestrict, XN[X<:NodeStore]<:Node[X]](in: D[_<:Node[NS],NS], xform: NodeSpell[XN]): D[XN[NS],NS]
}

trait NodeSpell[Out[X<:NodeStore]<:Node[X]] {
  def apply[NS<:NodeStore](in: Node[NS]): Out[NS]
}

class Aggregate[+N <: Node[NS], +NS <: NodeStore] extends Data[N, NS]

