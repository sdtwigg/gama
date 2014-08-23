package gama

object UInt {
  def apply(): UInt           = new UInt(None)
  def apply(width: Int): UInt = new UInt(Some(width))
  def apply(dir: IODirection): UInt = IOHelper.applyIO(apply(), dir)
  def apply(dir: IODirection, width: Int): UInt = IOHelper.applyIO(apply(width), dir)
}

class UInt(initial_width: Option[Int]) extends Bits(initial_width) {
}
