package gama

sealed trait NodeStore {def generic: NodeStore} // generic used by GenericizeSpell
sealed trait RawDigital extends NodeStore {def width: Option[Int]}
case class UBits(width: Option[Int]) extends RawDigital {def generic = UBits(None)}
case class SBits(width: Option[Int]) extends RawDigital {def generic = SBits(None)}
sealed trait RawAnalog extends NodeStore {def generic = this}
case object ClockNS extends RawAnalog

