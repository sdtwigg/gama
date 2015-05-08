package gama
package intermediate
package passes

object ExpandVSelectSink extends GamaPass {
  // This pass converts all instances of having a RefVSelect as a sink into
  //   a series of whens with RefVIndex as the sink
  // TODO: store the source as a ConstDecl?
  // TODO: Must be run after SubstituteAliases and ExplodeConnect (no AliasDecl or BiConnectStmt)
  def transform(target: ElaboratedModule): ElaboratedModule = {
    case class ConnectParams(source: ExprHW, details: ConnectDetails, note: GamaNote)
    def expandConnect(cmd: ConnectStmt): Iterable[CmdHW] = {
      // Search for the first RefVSelect in sink, if found, return a Ref tree with the RefVSelect replaced
      //   by a RefVIndex with a hole where the index is. Filling in the hole represented by function
      //   Also return the RefVSelect so its selector, note, etc. can be accessed later
      // If the reference has no RefVSelect, return None
      // Note the need to track the descent path
      // e.g. myB.a(ExprHW).b => myB.a(Int).b 
      def searchSink(sink: ExprHW): Option[Tuple2[Int=>RefHW, RefVSelect]] = sink match {
        // These all mark the end of a reference 'chain' (In fact, the Expr really shouldn't even show up...)
        case RefSymbol(_,_,_,_) | RefIO(_,_) | RefMSelect(_,_,_) | RefExprERROR(_) => None
        case ExprUnary(_,_,_,_) | ExprBinary(_,_,_,_,_) | ExprMux(_,_,_,_,_) | ExprLit(_,_,_) => None
        // These are possible parts of the chain so continue searching and ensure the result is appr wrapped
        case RefVIndex(source, idx, note) => searchSink(source).map({
          case (nsrc, refvs) => (i => RefVIndex(nsrc(i), idx, note), refvs)
        })
        case RefTLookup(source, field, note) => searchSink(source).map({
          case (nsrc, refvs) => (i => RefTLookup(nsrc(i), field, note), refvs)
        })
        case RefExtract(source, lp, rp, rType, note) => searchSink(source).map({
          case (nsrc, refvs) => (i => RefExtract(nsrc(i), lp, rp, rType, note), refvs)
        })
        // The desired root case:
        case refvs @ RefVSelect(source, selector, note) =>
          Some( ((i: Int) => RefVIndex(source, i, note), refvs) )
      }

      searchSink(cmd.sink) match {
        case Some((newsinkbuilder, oldrefvs)) => getVecDepth(oldrefvs.source.rType).map(depth => {
        //{getOrElse(throw new Exception("Malformed AST: RefVSelect on non-vec type")) // TODO: avoid exception?
          val newstmts = (0 until depth).map(i => {
            val newsink = newsinkbuilder(i)
            val cond = ExprBinary(OpEqual, oldrefvs.selector, ExprLitU(i), PrimitiveNode(UBits(Some(1))), GamaNote())
            WhenHW(cond, ConnectStmt(newsink, cmd.source, cmd.details, cmd.note), NOPHW, GamaNote())
          })
          newstmts.map(Transformer.transform(_))
        }).getOrElse(Some( CmdERROR("Malformed AST: Could not convert ConnectStmt: RefVSelect on non-vec type", cmd.note) ))

        case None => Some( cmd ) // Sink had no RefVSelect in it so just return current command
      }

    }
    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        case c @ ConnectStmt(_,_,_,_)   => expandConnect(c)

        case BiConnectStmt(_,_,_,note) => Some( CmdERROR("BiConnect found during ExpandVSelectSink", note) )
        case AliasDecl(_,_,note) => Some( CmdERROR("AliasDecl found during ExpandVSelectSink", note) )
        case _ => super.multiTransform(cmd)
      }
    }

    target.copy(body = Transformer.transform(target.body))
  }
}

