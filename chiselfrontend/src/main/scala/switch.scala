package gama

object switch {
  def apply(left: UIntLike)(ises: Seq[SwitchIs])(implicit em: EnclosingModule): Unit = {
    ises.foreach(is => {
      when.create(left === is.right, is.doWork, em)
    })
  }
}
object is {
  def apply(right: UIntLike)(work: =>Unit) = new SwitchIs(right, work)
}

protected[gama] class SwitchIs(val right: UIntLike, work: =>Unit) {
  def doWork(): Unit = work
}
