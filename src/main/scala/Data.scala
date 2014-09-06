package gama

// NOTE: AS OF CURRENT UNDERSTANDING: Data should NOT have a name var
//  it will have a name def that eventually propogates a name to the Node
//  Thus, as desired, name is lost after the shallow copy

// Node Descriptors - These are slightly mutable by design
//  immutable except node: that can only be assigned once
//  and direction, which is unaccessible 
abstract class Data {
  def copy: this.type
    // Should return an UNBOUND copy (and thus this is considered a shallow copy in other comments)

  protected[gama] def bind(spell: Element[NodeStore]=>Node[NodeStore])
    //   Spell should almost always use Element.generateStorage to return a new Node[NS<:NodeStore]
    //   Runtime errors will result if a bad spell is supplied
    // NOTE: Scala workaround: Type signature should really be Element[NS]=>Node[NS] with NS<:NodeStore

  def asInput: this.type
  def asOutput: this.type
  def flip: this.type

  protected[gama] def unsafeAssign(target: Data): this.type
  protected[gama] def unsafeMux(cond: Node[RawBits], tc: Data, fc: Data): Unit
}

object BindSpell {
  def wire = (e: Element[NodeStore]) => (new Wire(e.generateStorage))
  def port = (e: Element[NodeStore]) => (new Port(e.generateStorage, e.getDirection.getOrElse(
    throw new Exception("Cannot create Port from non-directioned Element"))))
}

