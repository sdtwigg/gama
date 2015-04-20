package gama
package internal

// For bi-connect
case class Left[+D<:Data](data: D) extends AnyVal
case class Right[+D<:Data](data: D) extends AnyVal

sealed trait BiConnectDetails
case object BiConnectToLeft extends BiConnectDetails // Used for Element and Aggregates when all fields connect
case object BiConnectToRight extends BiConnectDetails // Used for Element and Aggregates when all fields connect
case class  BiConnectTuple(fields: Seq[Tuple2[String,BiConnectDetails]]) extends BiConnectDetails
case class  BiConnectVec(elemdetails: BiConnectDetails) extends BiConnectDetails

// BOTH DIRECTION (BI) CONNECT
// TODO: Should the types both be contravariant?
// + Makes bundles somewhat more reasonable to connect up
// - might be strange for hwtuple (like From contravariance)
// TODO: Should there be only one type?
// + perhaps more sensible
// - potentially hurts expressibility for bundles
// - user can just be restrictive on not over-defining capabilities
//   so restriction may be unnecessary
@annotation.implicitNotFound("Cannot bidirectionally connect data between type ${LT} and type ${RT}. No implicit BiConnect[${LT},${RT}] resolvable.")
trait BiConnect[LT<:Data, RT<:Data] {
  def biDetails(left: Left[LT], right: Right[RT]): BiConnectDetails
  def biConnect(left: Left[LT], right: Right[RT], em: EnclosingModule): Unit
}
object BiConnect {
  def apply[LT<:Data,RT<:Data](implicit ev: BiConnect[LT, RT]) = ev
  trait BiConnectImpl[LT<:Data,RT<:Data] extends BiConnect[LT,RT] {
    def biConnect(left: Left[LT], right: Right[RT], em: EnclosingModule): Unit =
      em.getActiveJournal.append(BiConnectData(left, right, biDetails(left,right)))
  } // should this just be BiConnect?

  implicit def genBundleBiConnectBundle[LT<:Bundle,RT<:Bundle]: BiConnect[LT,RT] = new BundleBiConnectBundleImpl[LT,RT]{}

  def elemDetails(left: Node, right: Node): BiConnectDetails = {
    import DirectionIO.{Input, Output} // Using these extensively here so save some typing....
    val left_m: Module[_]  = left.oem.getOrElse(throw AmbiguousBiConnectException).enclosure
    val right_m: Module[_] = right.oem.getOrElse(throw AmbiguousBiConnectException).enclosure

    // CASE: left node is in parent module of right node
    if(right_m.parent.map(_ == left_m).getOrElse(false)) {
      // Thus, right node better be a port node and thus have a direction hint
      (left.resolveDirection, right.resolveDirection) match {
        //    PARENT MOD    CHILD MOD 
        case (Some(Input),  Some(Input))  => BiConnectToRight
        case (None,         Some(Input))  => BiConnectToRight

        case (Some(Output), Some(Output)) => BiConnectToLeft
        case (None,         Some(Output)) => BiConnectToLeft
        
        case (Some(Input),  Some(Output)) => throw AmbiguousBiConnectException
        case (Some(Output), Some(Input))  => throw AmbiguousBiConnectException
        case (_,            None)         => throw AmbiguousBiConnectException
      }
    }

    // CASE: right node is in parent module of left node (mirror of prior case)
    else if(left_m.parent.map(_ == right_m).getOrElse(false)) {
      // Thus, left node better be a port node and thus have a direction hint
      (left.resolveDirection, right.resolveDirection) match {
        //    CHILD MOD     PARENT MOD 
        case (Some(Input),  Some(Input))  => BiConnectToLeft
        case (Some(Input),  None)         => BiConnectToLeft

        case (Some(Output), Some(Output)) => BiConnectToRight
        case (Some(Output), None)         => BiConnectToRight

        case (Some(Input),  Some(Output)) => throw AmbiguousBiConnectException
        case (Some(Output), Some(Input))  => throw AmbiguousBiConnectException
        case (None, _)                    => throw AmbiguousBiConnectException
      }
    }

    // CASE: Both left node and right node in the same module
    else if(left_m == right_m) {
      (left.resolveDirection, right.resolveDirection) match {
        case (Some(Input),  Some(Output)) => BiConnectToRight
        case (Some(Input),  None)         => BiConnectToRight
        case (None,         Some(Output)) => BiConnectToRight

        case (Some(Output), Some(Input))  => BiConnectToLeft
        case (Some(Output), None)         => BiConnectToLeft
        case (None,         Some(Input))  => BiConnectToLeft

        case (Some(Input),  Some(Input))  => throw AmbiguousBiConnectException
        case (Some(Output), Some(Output)) => throw AmbiguousBiConnectException
        case (None,         None)         => throw AmbiguousBiConnectException
      }
    }
    
    // CASE: Both left node and right node in different modules with same parent
    else if((left_m != right_m) && left_m.parent == right_m.parent) {
      // Thus both nodes must be ports and have a direction hint
      (left.resolveDirection, right.resolveDirection) match {
        case (Some(Input),  Some(Output)) => BiConnectToLeft
        case (Some(Output), Some(Input))  => BiConnectToRight

        case (Some(Input),  Some(Input))  => throw AmbiguousBiConnectException
        case (Some(Output), Some(Output)) => throw AmbiguousBiConnectException
        case (_, None)                    => throw AmbiguousBiConnectException
        case (None, _)                    => throw AmbiguousBiConnectException
      }
    }

    else throw AmbiguousBiConnectException
  }
}
