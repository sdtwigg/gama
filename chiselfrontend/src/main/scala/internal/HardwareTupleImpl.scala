package gama
import internal._

trait HardwareTupleImpl extends SimpleCopy {
  self: HardwareTuple =>

  val subfields: Map[String, Data]
  lazy val subfields_ordered: Seq[Tuple2[String,Data]] = subfields.toSeq.sortBy(_._1)
    // lexicographically sorted

  private[this] def elements: Iterable[Data] = subfields.values
  protected[gama] def rebind(xform: NodeSpell[_<:Node]): this.type = {
    subfields.foreach({case (subfield: String, elem: Data) =>
      try {elem.rebind(xform)}
      catch {case e: Throwable => {throw TraversalException(subfield, this.getClass.getName, e)}}
    })
    this
  }
  protected[gama] def mimic(model: Data, asSPEC: Boolean): Unit = {
    model match {
      case hwt: HardwareTuple => {
        if(hwt.subfields.size != subfields.size) {throw StructuralMimicException}
        subfields.foreach({case (subfield: String, elem: Data) => {
          val model_elem: Data = hwt.subfields.get(subfield).getOrElse(throw StructuralMimicException)
          elem.mimic(model_elem, asSPEC)
        }})
      }
      case _ => throw StructuralMimicException
    }
  }

  def nodes: Seq[Node] = elements.flatMap(_.nodes).toSeq

  protected[gama] def propogateName(newname: NameTree, newsource: NameSource): Unit = {
    subfields.foreach({case (subfield: String, elem: Data) =>
      elem.forceSetName(NameField(this, subfield), newsource, true)
    })
  }
  protected[gama] def propogateDescRef(newdesc: Desc): Unit = elements.foreach( elem => {elem.setDescRef(newdesc, true)} )
}
