package gama
package frontend
package implementation

// NEVER PUT THIS IN AN IMPLICIT RESOLUTION PATH
// Only meant to be used by Bundle ConnectTo
case class RuntimeMisconnectException(sink: String, source: String)
  extends ChiselException(s"In a non-typechecked connect, improper attempted connection: tried to connect a $source to a $sink.")
object UnsafeConnectToDataImpl extends ConnectTo.ConnectToImpl[Data, Data] with BiConnect.BiConnectImpl[Data, Data] {
  def monoDetails(sink: Sink[Data], source: Source[Data]): ConnectDetails = {
    sink.data match {
      case left_e: Element => (source.data match {
        case right_e: Element => (ConnectAll)
        case _ => throw RuntimeMisconnectException(sink.data.getClass.getName,source.data.getClass.getName)
      }) // TODO: VERIFY ELEMENT CONNECTION SANE?
      case left_v: Vec[Data @unchecked] => (source.data match {
        case right_v: Vec[Data @unchecked] =>
          ( Vec.connectTo[Data,Data](this).monoDetails(Sink(left_v),Source(right_v)) )
        case _ => throw RuntimeMisconnectException(sink.data.getClass.getName,source.data.getClass.getName)
      })
      case left_t: HardwareTuple => (source.data match {
        case right_t: HardwareTuple => ( monoTuple(left_t, right_t) ) // in own function since so complicated
        case _ => throw RuntimeMisconnectException(sink.data.getClass.getName,source.data.getClass.getName)
      })
    }
  }
  def monoTuple(sink: HardwareTuple, source: HardwareTuple): ConnectDetails = {
    val candidates = sink.subfields_ordered
    val connect_list: Seq[Tuple2[String,ConnectDetails]] = candidates.flatMap({case (field, sink_elem) => {
      val source_elem_opt = source.subfields.get(field)
      val details = source_elem_opt.map(source_elem =>
        try { monoDetails(Sink(sink_elem), Source(source_elem)) }
        catch { case e: ChiselException => {throw TraversalException(field, sink.getClass.getName, e)} }
      )
      details.map((field, _))
    }})
    if( (connect_list.length == sink.subfields_ordered.length) &&
        (connect_list.length == source.subfields.size) &&
        connect_list.forall(_._2 == ConnectAll)
    ) { ConnectAll }
    else { ConnectTuple(connect_list) }
  }
  
  def biDetails(left: Left[Data], right: Right[Data], info: EnclosureInfo): BiConnectDetails = {
    left.data match {
      case left_e: Element => (right.data match {
        case right_e: Element => (BiConnect.elemDetails(left_e.node, right_e.node, info))
        case _ => throw RuntimeMisconnectException(left.data.getClass.getName,right.data.getClass.getName)
      }) // TODO: VERIFY ELEMENT CONNECTION SANE?
      case left_v: Vec[Data @unchecked] => (right.data match {
        case right_v: Vec[Data @unchecked] =>
          ( Vec.biConnect[Data,Data](this).biDetails(Left(left_v), Right(right_v), info) )
        case _ => throw RuntimeMisconnectException(left.data.getClass.getName,right.data.getClass.getName)
      })
      case left_t: HardwareTuple => (right.data match {
        case right_t: HardwareTuple => ( biTuple(left_t, right_t, info) ) // in own function since so complicated
        case _ => throw RuntimeMisconnectException(left.data.getClass.getName,right.data.getClass.getName)
      })
    }
  }
  def biTuple(left: HardwareTuple, right: HardwareTuple, info: EnclosureInfo): BiConnectDetails = {
    val candidates = left.subfields_ordered
    val connect_list: Seq[Tuple2[String,BiConnectDetails]] = candidates.flatMap({case (field, left_elem) => {
      val right_elem_opt = right.subfields.get(field)
      val details = right_elem_opt.map(right_elem =>
        try { biDetails(Left(left_elem), Right(right_elem), info) }
        catch { case e: ChiselException => {throw TraversalException(field, left.getClass.getName, e)} }
      )
      details.map((field, _))
    }})
    if( (connect_list.length == left.subfields_ordered.length) &&
        (connect_list.length == right.subfields.size) &&
        connect_list.forall(_._2 == BiConnectToLeft)
    ) { BiConnectToLeft }
    else if( (connect_list.length == left.subfields_ordered.length) &&
        (connect_list.length == right.subfields.size) &&
        connect_list.forall(_._2 == BiConnectToRight)
    ) { BiConnectToRight }
    else { BiConnectTuple(connect_list) }
  }
}
// TODO: The runtime walk is similar for both, can probably construct some factory methods for this....
