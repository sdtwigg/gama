package gama

trait NodeStore {def generic: NodeStore} // generic used by GenericizeSpell
sealed trait RawBits extends NodeStore {def width: Option[Int]}
case class UBits(width: Option[Int]) extends RawBits {def generic = UBits(None)}
case class SBits(width: Option[Int]) extends RawBits {def generic = SBits(None)}

