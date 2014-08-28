package gama

abstract class Aggregate extends Data {
  def getSubdata: IndexedSeq[Data]
    // This ordering MUST be consistent across calls and instances

  protected[gama] def flatten = getSubdata.map(_.flatten).reduce(_++_)

  protected[gama] def bind(spell: Element[NodeStore]=>Node[NodeStore]) = {
    getSubdata.foreach(_.bind(spell))
  }
  def asInput  = {getSubdata.foreach(_.asInput);  this}
  def asOutput = {getSubdata.foreach(_.asOutput); this}
  def flip     = {getSubdata.foreach(_.flip);     this}
}

class Vec[+T<:Data](subdata: IndexedSeq[T]) extends Aggregate {
  def getSubdata = subdata

  def copy = (new Vec(subdata.map(_.copy))).asInstanceOf[this.type]
}

