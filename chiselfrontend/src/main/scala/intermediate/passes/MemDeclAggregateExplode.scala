package gama
package intermediate
package passes

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

object MemDeclAggregateExplode extends GamaPass {
  val name = "MemDeclAggregateExplode"

  sealed trait MemPathTrace
  case class MPTStart(origin: MemDesc) extends MemPathTrace
  case class MPTField(previous: MemPathTrace, field: String) extends MemPathTrace
  case class MPTSelectOne(previous: MemPathTrace, index: Int) extends MemPathTrace

  def transform(target: ElaboratedModule): ElaboratedModule = {
    val memDescGen = new AppendableMemDescTable(target)

    val path2mem = HMap.empty[MemPathTrace, MemDesc]
    val mem2path = HMap.empty[MemDesc, MemPathTrace]
    val deadReferences = HSet.empty[MemDesc]
    
    // STEP 1: Explode all MemDecl into primitive types
    def explodeMemDecl(oldcmd: MemDecl): Iterable[CmdHW] = oldcmd.desc.mType match {
      case _: PrimitiveTypeHW => (Some( oldcmd ))
      case TupleHW(fields) => {
        deadReferences += oldcmd.desc // old target being exploded to invalidate its symbol
        fields.toSeq.sortBy(_._1).flatMap({case (field, eType) => {
          val newident = oldcmd.desc.identifier.getOrElse("") + "$" + field
          val newcmd = memDescGen.grantNewMem(memid =>
            MemDecl(MemDesc(memid, Some(newident), oldcmd.desc.depth, eType), oldcmd.note)
          )
          // Now, build reference replacement table entries for it
          val oldPath: MemPathTrace = mem2path.getOrElse(oldcmd.desc, MPTStart(oldcmd.desc))
          val newPath = MPTField(oldPath, field)
          val newDesc = newcmd.desc
          path2mem += newPath -> newDesc
          mem2path += newDesc -> newPath
          // Finally, act recursively
          explodeMemDecl(newcmd)
        }})
      }
      case VecHW(depth, eType) => {
        deadReferences += oldcmd.desc // old target being exploded to invalidate its symbol
        (0 until depth).flatMap(idx => {
          val newident = oldcmd.desc.identifier.getOrElse("") + "$" + idx
          val newcmd = memDescGen.grantNewMem(memid =>
            MemDecl(MemDesc(memid, Some(newident), oldcmd.desc.depth, eType), oldcmd.note)
          )
          // Now, build reference replacement table entries for it
          val oldPath: MemPathTrace = mem2path.getOrElse(oldcmd.desc, MPTStart(oldcmd.desc))
          val newPath = MPTSelectOne(oldPath, idx)
          val newDesc = newcmd.desc
          path2mem += newPath -> newDesc
          mem2path += newDesc -> newPath
          // Finally, act recursively
          explodeMemDecl(newcmd)
        })
      }
      case TypeHWUNKNOWN =>
        Some( CmdERROR(s"Unknown Type encountered during $name", oldcmd.note ) )
    }
    object DeclExplodeTransformer extends CmdMultiTransformTree {
      override def multiTransform(cmd: CmdHW): Iterable[CmdHW] = cmd match {
        case memD @  MemDecl(_,_) => explodeMemDecl(memD)
        case _ => super.multiTransform(cmd)
      }
    }
    val explodedDecl = target.copy(body = DeclExplodeTransformer.transform(target.body))
    
    // STEP 2: Ensure all aggregate sub-references distributed up as much as possible
    val smashedTarget = DistributeRef.transform(explodedDecl)
    
    // STEP 3: Replace references to Aggregate Mems using refines with references to Primitive Mems
    object RefReplaceTransformer extends ExprTransformTreeFullSegregation {
      def makePath(expr: ExprHW): Option[(MemPathTrace,RefMSelect)] = expr match {
        case rootMS @ RefMSelect(mem,_,_) => Some((MPTStart(mem), rootMS))
        
        case RefVIndex(source, index, _)  => makePath(source).map({case (srcPath, rootMS) => 
          (MPTSelectOne(srcPath, index), rootMS)
        })
        case RefTLookup(source, field, _) => makePath(source).map({case (srcPath, rootMS) => 
          (MPTField(srcPath, field), rootMS)
        })
        case _ => None
      }
      override def transform(ref: RefHW): RefHW  = ref match {
        case RefVIndex(_,_,_) | RefTLookup(_,_,_) => makePath(ref).flatMap({case (path, rootMS) =>
          path2mem.get(path) match {
            case Some(newMem) => Some( RefMSelect(newMem, rootMS.selector, rootMS.note) )
            case None => None
          }
        }).getOrElse(ref)

        case _ => super.transform(ref)
      }
    }
    val consistent = smashedTarget.copy(body = RefReplaceTransformer.transform(smashedTarget.body))
    
    // STEP 4: Make sure no references to dead symbols persist in the tree
    object DeadCheckTransformer extends ExprTransformTreeFullSegregation {
      override def transform(ref: RefHW): RefHW  = ref match {
        case rootMS @ RefMSelect(memDesc,_,_) =>
          if(deadReferences(memDesc)) RefExprERROR(s"Dead Reference not replaced during $name") 
          else rootMS
        case _  => super.transform(ref)
      }
    }

    consistent.copy(body = DeadCheckTransformer.transform(consistent.body))
  }
}
