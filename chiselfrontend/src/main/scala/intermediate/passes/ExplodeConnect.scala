package gama
package intermediate
package passes

object ExplodeConnect extends GamaPass {
  val name = "ExplodeConnect"
  def transform(target: ElaboratedModule): ElaboratedModule = {
    // This pass converts all ConnectStmt and BiConnectStmt in the module into ConnectStmt with only ConnectALL
    def explodeConnect(cmd: ConnectStmt): Iterable[ConnectStmt] = cmd.details match {
      case ConnectAll => Seq(cmd)
      case ConnectVec(numelems, elemdetails) => (0 until numelems).flatMap(idx => {explodeConnect(
        ConnectStmt(RefVIndex(cmd.sink, idx, cmd.note),
                    RefVIndex(cmd.source, idx, cmd.note),
                    elemdetails, cmd.note)
        )})
      case ConnectTuple(fields) => fields.flatMap({case (field, elemdetails) => {explodeConnect(
        ConnectStmt(RefTLookup(cmd.sink, field, cmd.note),
                    RefTLookup(cmd.source, field, cmd.note),
                    elemdetails, cmd.note)
        )}})
    }
    def explodeBiConnect(cmd: BiConnectStmt): Iterable[ConnectStmt] = cmd.details match {
      case BiConnectToLeft  => explodeConnect(ConnectStmt(cmd.left, cmd.right, ConnectAll, cmd.note))
      case BiConnectToRight => explodeConnect(ConnectStmt(cmd.right, cmd.left, ConnectAll, cmd.note))
      case BiConnectVec(numelems, elemdetails) => (0 until numelems).flatMap(idx => {explodeBiConnect(
        BiConnectStmt(RefVIndex(cmd.left, idx, cmd.note),
                      RefVIndex(cmd.right, idx, cmd.note),
                      elemdetails, cmd.note)
        )})
      case BiConnectTuple(fields) => fields.flatMap({case (field, elemdetails) => {explodeBiConnect(
        BiConnectStmt(RefTLookup(cmd.left, field, cmd.note),
                      RefTLookup(cmd.right, field, cmd.note),
                      elemdetails, cmd.note)
        )}})
    }
    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        case  c @ ConnectStmt(_,_,_,_)   => explodeConnect(c)
        case bc @ BiConnectStmt(_,_,_,_) => explodeBiConnect(bc)

        case _ => super.multiTransform(cmd)
      }
    }

    target.copy(body = Transformer.transform(target.body))
  }
}

