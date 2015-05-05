package gama
package frontend
package implementation

// SINGLE DIRECTION (MONO) CONNECT
// TODO: Global Data := Data assign added via implicit class that furthermore uses implicit ConnectTo?
// As of right now, must sort-of make sure ConnectTo typeclasses added along with new := so that Vec remains sane
// TODO: Should the contravariance exist?
// + does prevent creating redundant ConnectTo
//   e.g. ConnectTo[UInt, BaseElem] can take place of ConnectTo[UInt, UInt/SInt/Bool/...]
// - that might be strange for hwtuple
@annotation.implicitNotFound("Cannot connect data of type ${From} to data of type ${To}. No implicit ConnectTo[${To},${From}] resolvable.")
trait ConnectTo[To<:Data, -From<:Data] {
  def monoDetails(sink: Sink[To], source: Source[From], info: EnclosureInfo): ConnectDetails
  // TODO: Better facilities for when want to do logic on the source before connect
  //  e.g. a fixed point class that may need to shift or extract first. Note that the connect details returned
  //    would likely have a 'hole' where that class's fields would be connected, then this returns a list
  //    of other entries to append after the connect. Note, need separate channel so this info also propogates
  //    to bundle
  //  Note: SInt := UInt does not have to use this feature... width inference currently can do a 1 bit zext
  //    However, if this is added, then the width inference functionality would just be skipped (as SInt would
  //    no longer be connected to UInt but toSInt(UInt), which is an SInt)`
  def monoConnect(sink: Sink[To], source: Source[From], info: EnclosureInfo): Unit = 
    info.em.getActiveJournal.append(journal.ConnectData(sink, source, monoDetails(sink,source,info), info))
  protected[gama] def preciseMonoConnect(sink: Sink[To], source: Source[From], info: EnclosureInfo,
    targetloc: Tuple2[journal.Journal, journal.Entry]): Unit =
  {
    targetloc._1.insertAfter(journal.ConnectData(sink, source, monoDetails(sink,source,info), info), targetloc._2)
  }
  // Use for in the VERY RARE cases that need to override the journal and entry position written to
  //   e.g. Module uses this to ensure the reset for a child is connected in precisely the right place
  //        regardless of usercode insanity (likely, abuse of lazy val or def in that child module)
  // USE WITH EXTREME CAUTION as it is not ensured journal is even on the EnclosingModule's journal stack
}
object ConnectTo {
  type From[D<:Data] = {type CB[To<:Data] = ConnectTo[To, D]}
  // context bound of form D<:Data: ConnectTo.From[UInt]#CB means
  //   supplied D must be able to connect from a UInt

  type Self = {type CB[D<:Data] = ConnectTo[D, D]}
  // context bound of form D<:Data: ConnectTo.Self#CB means
  //   supplied D must be able to connect from D (itself)

  def apply[To<:Data,From<:Data](implicit ev: ConnectTo[To, From]) = ev

  implicit def genBundleConnectToBundle[B<:Bundle]: ConnectTo[B,Bundle] = new BundleConnectToBundleImpl[B]{}

  def elemDetails(sink: Node, source: Node, info: EnclosureInfo): ConnectDetails = {
    import DirectionIO.{Input, Output} // Using these extensively here so save some typing....
    val context_m: Module[_<:Data] = info.em.enclosure
    val sink_m: Module[_<:Data]    = sink.oem.map(_.enclosure).getOrElse(context_m)
    val source_m: Module[_<:Data]  = source.oem.map(_.enclosure).getOrElse(context_m)
    
    // CASE: Context is same module that both left node and right node are in
    if( (context_m == sink_m) && (context_m == source_m) ) {
      (sink.resolveDirection, source.resolveDirection) match {
        //   SINK           SOURCE
        //   CURRENT MOD    CURRENT MOD
        case (Some(Output), _) => ConnectAll
        case (None,         _) => ConnectAll
        case (Some(Input),  _) => throw MonoConnectDirectionException 
      }
    }

    // CASE: Context is same module as sink node and right node is in a child module
    else if( (sink_m == context_m) && (source_m.parent.map(_ == context_m).getOrElse(false)) ) {
      // Thus, right node better be a port node and thus have a direction hint
      (sink.resolveDirection, source.resolveDirection) match {
        //    SINK          SOURCE
        //    CONTEXT MOD    CHILD MOD 
        case (None,         Some(Output)) => ConnectAll
        case (None,         Some(Input))  => ConnectAll
        case (Some(Output), Some(Output)) => ConnectAll
        case (Some(Output), Some(Input))  => ConnectAll
        case (_,            None) => throw CrossModuleAccessException
        case (Some(Input),  _)    => throw MonoConnectDirectionException
      }
    }

    // CASE: Context is same module as source node and sink node is in child module
    else if( (source_m == context_m) && (sink_m.parent.map(_ == context_m).getOrElse(false)) ) {
      // Thus, left node better be a port node and thus have a direction hint
      (sink.resolveDirection, source.resolveDirection) match {
        //    SINK          SOURCE
        //    CHILD MOD     CONTEXT MOD 
        case (Some(Input),  _) => ConnectAll
        case (Some(Output), _) => throw MonoConnectDirectionException 
        case (None,         _) => throw CrossModuleAccessException
      }
    }

    
    // CASE: Context is the parent module of both the module containing sink node
    //                                        and the module containing source node
    //   Note: This includes case when sink and source in same module but in parent
    else if( (sink_m.parent.map(_ == context_m).getOrElse(false)) &&
             (source_m.parent.map(_ == context_m).getOrElse(false)) 
    ) {
      // Thus both nodes must be ports and have a direction hint
      (sink.resolveDirection, source.resolveDirection) match {
        //    SINK          SOURCE
        //    CHILD MOD     CHILD MOD
        case (Some(Input),  Some(Input))  => ConnectAll
        case (Some(Input),  Some(Output)) => ConnectAll
        case (Some(Output), _)            => throw MonoConnectDirectionException 
        case (_,            None)         => throw CrossModuleAccessException
        case (None,         _)            => throw CrossModuleAccessException
      }
    }

    else throw CrossModuleAccessException
  }
}

