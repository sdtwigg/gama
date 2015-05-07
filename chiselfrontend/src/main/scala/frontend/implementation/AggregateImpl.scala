package gama
package frontend
package implementation

abstract class AggregateImpl {
  self: Aggregate =>

  protected[gama] def elements: Seq[Data]
  
  def nodes = elements.flatMap(_.nodes)

  def getWidth: Option[Int] = elements.map(_.getWidth).foldLeft(Option(0))((oacc, oelem) => {
    for(acc <- oacc; elem <- oelem) yield acc + elem
  })
  
  def do_asUInt(info: EnclosureInfo): UInt =
    if(elements.length > 0) elements.reverse.map(_.do_asUInt(info)).reduceLeft((left, right) => left.do_cat(right, info))
    else LiteralUInt(0,0)
  // TODO: This code is effectively duplicated in Cat.scala

  def do_connectFromUInt(in: UInt, info: EnclosureInfo): this.type = {
    require(getWidth.isDefined, "Cannot use fromUInt on Aggregate where some element has unknown width.")
    val elem_widths = elements.map(_.getWidth.get)
    val slice_borders = elem_widths.scanLeft(0)(_+_)
    elements.zipWithIndex.foreach({case (elem: Data, idx: Int) =>
      if(elem_widths(idx) > 0)
        elem.do_connectFromUInt(in.doExtract(slice_borders(idx+1)-1, slice_borders(idx), info) ,info)
    })
    this
  }
}
