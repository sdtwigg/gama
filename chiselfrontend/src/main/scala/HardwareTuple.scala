package gama
import internal._

abstract class HardwareTuple extends Data {
  protected[gama] val subfields: Seq[Tuple2[String, Data]]

  private[this] def elements: Seq[Data] = subfields.map(_._2)
  protected[gama] def rebind(xform: NodeSpell[_<:Node]): this.type = {
    subfields.foreach({case (subfield: String, elem: Data) =>
      try {elem.rebind(xform)}
      catch {case e: Throwable => {throw TraversalException(subfield, this.getClass.getName, e)}}
    })
    this
  }

  def nodes: Seq[Node] = elements.flatMap(_.nodes)

  protected[gama] def propogateName(newname: NameTree, newsource: NameSource): Unit = {
    subfields.foreach({case (subfield: String, elem: Data) =>
      elem.forceSetName(NameField(newname, subfield), newsource, true)
    })
  }
  protected[gama] def propogateDescRef(newdesc: Desc): Unit = elements.foreach( elem => {elem.setDescRef(newdesc, true)} )
}
