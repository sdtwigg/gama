package gama
package intermediate
package passes

object ConnectAggregateExplode extends GamaPass {
  val name = "ConnectAggregateExplode"
  def transform(target: ElaboratedModule): ElaboratedModule = {
    // This pass converts all ConnectStmt on Aggregates into multiple on primitives
    // TODO: Assumes ExplodeConnect has been run
    def explodeConnect(cmd: ConnectStmt): Iterable[CmdHW] = cmd.sink.rType match {
      case _: PrimitiveTypeHW => (Some( cmd ))
      case TupleHW(fields) => fields.keys.toSeq.sorted.flatMap(field => {
        val newcmd = ConnectStmt(RefTLookup(cmd.sink, field, cmd.note),
                                 RefTLookup(cmd.source, field, cmd.note),
                                 ConnectAll, cmd.note)
        explodeConnect(newcmd)
      })
      case VecHW(depth, _) => (0 until depth).flatMap(idx => {
        val newcmd = ConnectStmt(RefVIndex(cmd.sink, idx, cmd.note),
                                 RefVIndex(cmd.source, idx, cmd.note),
                                 ConnectAll, cmd.note)
        explodeConnect(newcmd)
      })
      case TypeHWUNKNOWN => Some( CmdERROR(s"Unknown Type encountered during $name", cmd.note ) )
    }
    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        case c @ ConnectStmt(_,_,details,_) if details == ConnectAll  => explodeConnect(c)
        case ConnectStmt(_,_,_,note) =>
          Some( CmdERROR(s"non-ConnectAll Connect found during $name", note) )
        case BiConnectStmt(_,_,_,note) =>
          Some( CmdERROR(s"BiConnect found during $name", note) )

        case _ => super.multiTransform(cmd)
      }
    }

    target.copy(body = Transformer.transform(target.body))
  }
}

