package gama

class Element[+N <: Node[NS], +NS <: NodeStore](val node: N with Node[NS]) extends Data[N, NS]

class Bits[+N <: Node[NS], +NS <: RawBits](node: N with Node[NS]) extends Element(node)
object Bits {
  implicit object bitsRegenerator extends Regenerate[Bits,RawBits] {
    def regenerate[NS<:RawBits, XN[X<:NodeStore]<:Node[X]](in: Bits[_<:Node[NS],NS], xform: NodeSpell[XN]): Bits[XN[NS],NS] = new Bits(xform(in.node))
  }
}

