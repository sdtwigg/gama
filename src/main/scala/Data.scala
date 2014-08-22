package gama

trait IODirection
object INPUT  extends IODirection
object OUTPUT extends IODirection

// Node Descriptors - These are immutable by design
abstract class Data {
  def regenerate(spell: Node[NodeStore]=>Node[NodeStore]): this.type
    // NOTE: Scala workaround: Type signature should be Node[NS]=>Node[NS] with NS<:NodeStore, casts used later under this assumption
    //   Runtime errors will result if a bad spell is supplied
    
  def asInput:  this.type = regenerate((n: Node[NodeStore]) => (new Port(n.storage, INPUT)))
  def asOutput: this.type = regenerate((n: Node[NodeStore]) => (new Port(n.storage, OUTPUT)))
}

abstract class Element[NS<:NodeStore](val node: Node[NS]) extends Data {
  final def regenerate(spell: Node[NodeStore]=>Node[NodeStore]) = regenerate(spell(node).asInstanceOf[Node[NS]])
    // asInstanceOf typecast necessary due to Note on declaration of prototype
  def regenerate(newtarget: Node[NS]): this.type
}

abstract class Bits(node: Node[RawBits]) extends Element(node) {
}

class UInt(node: Node[RawBits]) extends Bits(node) {
  def regenerate(newtarget: Node[RawBits]) = (new UInt(newtarget)).asInstanceOf[this.type]
}
