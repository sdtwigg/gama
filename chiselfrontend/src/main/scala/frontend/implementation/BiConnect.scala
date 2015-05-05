package gama
package frontend
package implementation

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
  def biDetails(left: Left[LT], right: Right[RT], info: EnclosureInfo): BiConnectDetails
  // TODO: See discussion in ConnectTo on hole-y connects with injected statements
  def biConnect(left: Left[LT], right: Right[RT], info: EnclosureInfo): Unit = 
    info.em.getActiveJournal.append(journal.BiConnectData(left, right, biDetails(left,right, info), info))
}
  // Note: em for biDetails is required to resolve when connecting same module's input to output
  //   if inside the module
object BiConnect {
  type With[Right<:Data] = {type CB[Left<:Data] = BiConnect[Left, Right]}
  // context bound of form D<:Data: BiConnect.From[UInt]#CB means
  //   supplied D must be able to bi-connect with a UInt (on the right)

  type Self = {type CB[D<:Data] = BiConnect[D, D]}
  // context bound of form D<:Data: BiConnect.Self#CB means
  //   supplied D must be able to bi-connect from D (itself)

  implicit def reverser[LT<:Data,RT<:Data](implicit ev: BiConnect[LT, RT]): BiConnect[RT,LT] = new BiConnect[RT,LT] {
    def biDetails(left: Left[RT], right: Right[LT], info: EnclosureInfo): BiConnectDetails =
      ev.biDetails(Left(right.data), Right(left.data), info).flipped
  }
  // TODO: IS THIS OK? Probably, BiConnect should be totally commutative
  
  def apply[LT<:Data,RT<:Data](implicit ev: BiConnect[LT, RT]) = ev

  implicit def genBundleBiConnectBundle[LT<:Bundle,RT<:Bundle]: BiConnect[LT,RT] = new BundleBiConnectBundleImpl[LT,RT]{}

  def elemDetails(left: Node, right: Node, info: EnclosureInfo): BiConnectDetails = {
    import DirectionIO.{Input, Output} // Using these extensively here so save some typing....
    val context_m: Module[_<:Data] = info.em.enclosure
    val left_m: Module[_<:Data]    = left.oem.map(_.enclosure).getOrElse(context_m)
    val right_m: Module[_<:Data]   = right.oem.map(_.enclosure).getOrElse(context_m)

    // CASE: Context is same module as left node and right node is in a child module
    if( (left_m == context_m) && (right_m.parent.map(_ == context_m).getOrElse(false)) ) {
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

    // CASE: Context is same module as right node and left node is in child module
    else if( (right_m == context_m) && (left_m.parent.map(_ == context_m).getOrElse(false)) ) {
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

    // CASE: Context is same module that both left node and right node are in
    else if( (context_m == left_m) && (context_m == right_m) ) {
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
    
    // CASE: Context is the parent module of both the module containing left node
    //                                        and the module containing right node
    //   Note: This includes case when left and right in same module but in parent
    else if( (left_m.parent.map(_ == context_m).getOrElse(false)) &&
             (right_m.parent.map(_ == context_m).getOrElse(false)) 
    ) {
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
