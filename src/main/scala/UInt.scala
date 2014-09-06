package gama

object UInt {
  def apply(): UInt           = new UInt(None)
  def apply(width: Int): UInt = new UInt(Some(width))
  def apply(dir: IODirection): UInt = IOHelper.applyIO(apply(), dir)
  def apply(dir: IODirection, width: Int): UInt = IOHelper.applyIO(apply(width), dir)
}

class UInt(initial_width: Option[Int]) extends Bits(initial_width) {
  def copy = (new UInt(initial_width)).asInstanceOf[this.type]
  
  // Arithmetic on UInt
  def +(target: UInt): UInt = BinaryOp(UInt(), this, target, Opcodes.UPlus)

  // Debugging purposes ONLY
  override def toString = {
    s"<UInt:w=${initial_width},d=${getDirection},bound=${isBound}>"
  }
}

class Bool extends UInt(Some(1)) {
  override def copy = (new Bool).asInstanceOf[this.type]
}
