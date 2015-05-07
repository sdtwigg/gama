package gama
package frontend
package implementation

trait HardwareTupleImpl extends SimpleCopy {
  self: HardwareTuple =>

  val subfields: Map[String, Data]
  lazy val subfields_ordered: Seq[Tuple2[String,Data]] = subfields.toSeq.sortBy(_._1)
    // lexicographically sorted

  protected[gama] def elements: Seq[Data] = subfields_ordered.map(_._2)
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

  protected[gama] def propogateName(newname: NameTree, newsource: NameSource): Unit = {
    subfields.foreach({case (subfield: String, elem: Data) =>
      elem.forceSetName(NameField(this, subfield), newsource, true)
    })
  }
  protected[gama] def propogateDescRef(newdesc: Desc): Unit = subfields.values.foreach( elem => {elem.setDescRef(newdesc, true)} )
}

