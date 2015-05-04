package gama
package frontend

import implementation._

object switch {
  def apply(left: UIntLike)(ises: Seq[SwitchIs])(implicit em: EnclosingModule): Unit = {
    ises.foreach(is => {
      when.create(left.do_eq(is.right, EnclosureInfo(em, None)), is.doWork, em)
    })
    // TODO: CONSIDER: Should EnclosureInfo be totally filled in? How
  }
}
object is {
  def apply(right: UIntLike)(work: =>Unit): SwitchIs = new SwitchIs(right, work)
}

protected[gama] class SwitchIs(val right: UIntLike, work: =>Unit) {
  def doWork(): Unit = work
}
