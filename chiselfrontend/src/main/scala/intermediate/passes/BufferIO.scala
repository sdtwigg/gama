package gama
package intermediate
package passes

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

object BufferIO extends GamaPass {
  val name = "BufferIO"
  def transform(target: ElaboratedModule): ElaboratedModule = {
    val symbolGen = new AppendableRefSymbolTable(target)

    val ref2sym  = HMap.empty[ModuleRef, RefSymbol]
    val sym2ref  = HMap.empty[RefSymbol, ModuleRef]

    // STEP 1: Create buffer wire for own IOs
    val selfBuff: WireDecl = {
      val buffident = Some("$$this$IO")
      val bufftype = typePort2Node(target.io)
      val newbuff = symbolGen.grantNewSymbol(id =>
        WireDecl(RefSymbol(id, buffident, bufftype, passNote), passNote)
      )
      ref2sym += ModuleThis(target.io) -> newbuff.symbol
      sym2ref += newbuff.symbol -> ModuleThis(target.io)
      newbuff
    }
    val ownBuffered = target.copy(body = target.body match {
      case BlockHW(cmds, note) => BlockHW(selfBuff :: cmds, note)
      case cmd => BlockHW(selfBuff :: cmd :: Nil, passNote)
    })

    // STEP 2: Create a buffer wire for submodule IOs
    object CreateBufferTransformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW): Iterable[CmdHW] = cmd match {
        case submod @ SubModuleDecl(details, _, _) => {
          val buffident = Some(details.identifier.getOrElse("") + "$IO" )
          val bufftype = typePort2Node(details.ioType)
          val newbuff = symbolGen.grantNewSymbol(id =>
            WireDecl(RefSymbol(id, buffident, bufftype, passNote), passNote)
          )
          ref2sym += details -> newbuff.symbol
          sym2ref += newbuff.symbol -> details
          Seq(submod, newbuff)
        }
        case _ => super.multiTransform(cmd)
      }
    }
    val submodBuffered = CreateBufferTransformer.transform(ownBuffered)

    // STEP 3: Replace all references with RefIO into reference to the buffer wire
    object RefIOReplaceTransformer extends ExprTransformTreeFullSegregation {
      override def transform(ref: RefHW): RefHW = ref match {
        case RefIO(mod, _) => ref2sym.get(mod).getOrElse(RefExprERROR(s"Dead reference encountered during $name"))
        case _ => super.transform(ref)
      }
    }
    val refReplaced = RefIOReplaceTransformer.transform(submodBuffered)

    // STEP 4: Add a BiConnectStmt between the IO and buffer wire
    def buildDetails(port: TypeHW, isSubMod: Boolean): BiConnectDetails = port match {
      // Assuming port is on right
      case PrimitivePort(_, DirectionIO.Input)  => if(isSubMod) BiConnectToRight else BiConnectToLeft
      case PrimitivePort(_, DirectionIO.Output) => if(isSubMod) BiConnectToLeft else BiConnectToRight
      case PrimitiveNode(_) | TypeHWUNKNOWN => throw new Exception("Catastrophic Internal Error")

      case VecHW(depth, eType) => {
        val eDetails = buildDetails(eType, isSubMod)
        if(eDetails == BiConnectToLeft) BiConnectToLeft
        else if(eDetails == BiConnectToRight) BiConnectToRight
        else BiConnectVec(depth, eDetails)
      }
      case TupleHW(fields) => {
        val subdetails = fields.toSeq.sortBy(_._1).map({case (field, eType) =>
          (field, buildDetails(eType, isSubMod))
        })
        if(subdetails.forall(_._2 == BiConnectToLeft)) BiConnectToLeft
        else if(subdetails.forall(_._2 == BiConnectToRight)) BiConnectToRight
        else BiConnectTuple(subdetails)
      }
    }
    object BufferWireIOConnect extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW): Iterable[CmdHW] = cmd match {
        case wiredecl @ WireDecl(symbol, _) if sym2ref.isDefinedAt(symbol) => {
          val modref = sym2ref(symbol)
          val isSubMod = modref match {case ModuleThis(_) => false; case ModuleSub(_,_,_) => true}
          val newbicnct = BiConnectStmt(
            symbol, RefIO(modref, passNote),
            buildDetails(modref.ioType, isSubMod),
            passNote
          )
          Seq(wiredecl, newbicnct)
        }
        case _ => super.multiTransform(cmd)
      }
    }

    BufferWireIOConnect.transform(refReplaced)
  }
}
