package gama
package intermediate
package passes

object ExpandMSelect extends PassMerger(Vector(ExpandMSelectSink, ExpandMSelectSource))

object ExpandMSelectSink extends GamaPass {
  val name = "ExpandMSelectSink"
  // This pass converts all instances of having a RefMSelect as a sink into MemWrite
  // TODO: Must be run after SubstituteAliases, ExplodeConnect, and DeclAggregateExplode
  //   (no AliasDecl or BiConnectStmt or sub-referenced aggregate mems)
  // Note, may still have a RefExtract to contend with which turns into a masked write
  def transform(target: ElaboratedModule): ElaboratedModule = {
    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        //case c @ ConnectStmt(_,_,_,_)   => expandConnect(c)

        case BiConnectStmt(_,_,_,note) => Some( CmdERROR(s"BiConnect found during $name", note) )
        case AliasDecl(_,_,note) => Some( CmdERROR(s"AliasDecl found during $name", note) )
        case _ => super.multiTransform(cmd)
      }
    }

    Transformer.transform(target)
  }
}

object ExpandMSelectSource extends GamaPass {
  val name = "ExpandMSelectSource"
  // This pass converts all instances of having a RefMSelect as a source into MemRead
  // TODO: Requires RemoveAliases and ExplodeConnects to have been run
  // TODO: Create Mux tree instead of Mux chain
  def transform(target: ElaboratedModule): ElaboratedModule = {
    object Transformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW) = cmd match {
        //case ConnectStmt(sink, source, details, note) => ConnectStmt(sink, transform(source), details, note)
          // Don't transform sink!!! ExpandMSelectSink does this
        case BiConnectStmt(_,_,_,note) => Some( CmdERROR(s"BiConnect found during $name", note) )
        case AliasDecl(_,_,note) => Some( CmdERROR(s"AliasDecl found during $name", note) )
        case _ => super.multiTransform(cmd)
      }
    }

    Transformer.transform(target)
  }
}

