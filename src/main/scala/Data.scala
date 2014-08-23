package gama

// Node Descriptors - These are slightly mutable by design
//  immutable except node: that can only be assigned once
//  and direction, which is unaccessible 
abstract class Data {
  protected[gama] def bind(spell: Element[NodeStore]=>Node[NodeStore])
    //   Spell should almost always use Element.generateStorage to return a new Node[NS<:NodeStore]
    // NOTE: Scala workaround: Type signature should really be Element[NS]=>Node[NS] with NS<:NodeStore
    //   Runtime errors will result if a bad spell is supplied
  def asInput: this.type
  def asOutput: this.type
  def flip: this.type
}

