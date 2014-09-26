package gama

abstract class Element[+N <: Node[NS], +NS <: NodeStore](val node: N with Node[NS]) extends Data[N, NS]

abstract class Bits[+N <: Node[NS], +NS <: RawBits](node: N with Node[NS]) extends Element(node)
/*
// Muxer for Bits (but tricky to use)
object Bits {
  implicit object muxer extends SelfMuxable[Bits,RawBits] {
    def mux(left: Bits[_<:Node[_<:RawBits],_<:RawBits], right: Bits[_<:Node[_<:RawBits],_<:RawBits]) = new UInt(new Mux(RawUBits(None)))
  }
}
*/

class SInt[+N <: Node[NS], +NS <: RawSBits](node: N with Node[NS]) extends Bits(node)
object SInt {
  implicit object regenerator extends Regenerate[SInt,RawSBits] {
    def regenerate[NS<:RawSBits, XN[X<:NodeStore]<:Node[X]](in: SInt[_<:Node[NS],NS], xform: NodeSpell[XN]) = new SInt(xform(in.node))
  }
  implicit object muxer extends SelfMuxable[SInt,RawSBits] {
    def mux(left: SInt[_<:Node[_<:RawSBits],_<:RawSBits], right: SInt[_<:Node[_<:RawSBits],_<:RawSBits]) = new SInt(new Mux(RawSBits(None)))
  }
}

class UInt[+N <: Node[NS], +NS <: RawUBits](node: N with Node[NS]) extends Bits(node)
object UInt {
  implicit object regenerator extends Regenerate[UInt,RawUBits] {
    def regenerate[NS<:RawUBits, XN[X<:NodeStore]<:Node[X]](in: UInt[_<:Node[NS],NS], xform: NodeSpell[XN]) = new UInt(xform(in.node))
  }
  implicit object muxer extends SelfMuxable[UInt,RawUBits] {
    def mux(left: UInt[_<:Node[_<:RawUBits],_<:RawUBits], right: UInt[_<:Node[_<:RawUBits],_<:RawUBits]) = new UInt(new Mux(RawUBits(None)))
  }
}

