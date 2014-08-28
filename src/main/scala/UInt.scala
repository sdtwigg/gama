package gama

object UInt {
  def apply(): UInt           = new UInt(None)
  def apply(width: Int): UInt = new UInt(Some(width))
  def apply(dir: IODirection): UInt = IOHelper.applyIO(apply(), dir)
  def apply(dir: IODirection, width: Int): UInt = IOHelper.applyIO(apply(width), dir)
}

class UInt(initial_width: Option[Int]) extends Bits(initial_width) {
  def copy = {
    val mycopy = new UInt(initial_width)
    mycopy.copyDirection(this)
    mycopy.asInstanceOf[this.type]
  }

  def :=(target: UInt): UInt = {
    this.handleAssign(target.getNode)
    this
  }

  def +(target: UInt): UInt = BinaryOp(UInt(), this, target, Opcodes.UPlus)

  override def toString = {
    s"<UInt:w=${initial_width},d=${getDirection},bound=${isBound}>"
  }
}
