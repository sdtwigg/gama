package gama
package intermediate
package passes

import scala.collection.mutable.{HashMap=>HMap, HashSet=>HSet, ListBuffer=>ListB}

object CircuitFlattenIO {
  // Flattens all IOs such that all modules have an IO that is TupleHW with fields of only PrimitivePort
  // TODO: Assumes RefVSelect, InternalAggregateExplode already run on each module
  // (Otherwise, issues with replacing RefIO)
  val name = "CircuitFlattenIO"
  val passNote = GamaNote(GTSourcePass(name))
  
  def transform(target: ElaboratedCircuit): ElaboratedCircuit = {
    // STEP 1: for each module, decompose IO into primitives
    
    // Output type is new ports (primitives only) types, names (maybe), and builder for the old port path
    def flattenIO(old: TypeHW, oldBuilder: ModuleRef=>IOPathTrace
                 ): Iterable[(PrimitivePort, Option[String], ModuleRef=>IOPathTrace)] = old match {
      case port @ PrimitivePort(_,_) =>
        Some(( port, None, oldBuilder ))

      case VecHW(depth, elemType) => for {
        idx <- 0 until depth
        aggBuilder = (ref: ModuleRef) => IPTSelectOne(oldBuilder(ref), idx)
        // Now, handle inner eType
        (eType: PrimitivePort, eoName, eBuilder) <- flattenIO(elemType, aggBuilder)
        newName = idx + eoName.map("$"+_).getOrElse("")
      } yield (eType, Some(newName), eBuilder)

      case TupleHW(fields) => for {
        (field, elemType) <- fields.toSeq.sortBy(_._1)
        aggBuilder = (ref: ModuleRef) => IPTField(oldBuilder(ref), field)
        // Now, handle inner eType
        (eType: PrimitivePort, eoName, eBuilder) <- flattenIO(elemType, aggBuilder)
        newName = field + eoName.map("$"+_).getOrElse("")
      } yield (eType, Some(newName), eBuilder)

      case PrimitiveNode(_) | TypeHWUNKNOWN => throw new Exception(s"Catastrophic Error: Bad IO type during $name") 
    }
    type TransBuilder = ModuleRef=>(IOPathTrace,RefHW)
    // Return new IO type and function to build translation table from oldpath to newref using oldmodref and newmodref
    val simplifiedIOs: Vector[(TupleHW, (ModuleRef,ModuleRef)=>Iterable[(IOPathTrace,RefHW)])] =
      target.modules.map(module => {
        val usedFields = HSet.empty[String]
        val flatIO: Iterable[(PrimitivePort, String, ModuleRef=>IOPathTrace)] =
          flattenIO(module.io, (ref: ModuleRef) => IPTStart(ref)).map({case (eType, oName, ref) => {
            // Now, make sure each IO entry has an OK, unique name...
            // Name cannot be "" or start with a digit
            val candName = oName.map(x => x.headOption match {
              case None => "IO"
              case Some(num) if num.isDigit => "IO$" + x
              case _ => x
            }).getOrElse("IO")
            val name = if(usedFields(candName)) {
              (for {
                idx <- Stream.from(1)
                newCand = candName + "$$" + idx.toString
                if !usedFields(newCand)
              } yield newCand).head
            } else candName
            usedFields += name

            (eType, name, ref)
          }})

        val newIO = TupleHW(flatIO.map({case (eType, field, _) => (field, eType)}).toMap)
        val transBuilder = (oldref: ModuleRef, newref: ModuleRef) => flatIO.map({case (_, newfield, oldPath) => 
          (oldPath(oldref), RefTLookup(RefIO(newref, passNote), newfield, passNote))
        })
        (newIO, transBuilder)
      })

    // STEP 2: For each module, go through and replace all RefIOs, ModuleRefs using new IO and path information
    val updatedModules = target.modules.zipWithIndex.map({case (ElaboratedModule(selftype, oldIO, oldBody), selfCircuitIdx) => {
      val (newIO, selfTransBuilder) = simplifiedIOs(selfCircuitIdx)
      val oldSelfRef = ModuleThis(oldIO)
      val newSelfRef = ModuleThis(newIO)

      val path2ref = HMap.empty[IOPathTrace, RefHW] // will use the TransBuilders to populate this table...
      selfTransBuilder(oldSelfRef, newSelfRef).foreach({case (oldPath, newRef) => path2ref += oldPath -> newRef})

      object Transformer extends ExprTransformTreeFullSegregation {
        override def transform(cmd: CmdHW): CmdHW = cmd match {
          case SubModuleDecl(oldSubRef, circuitptr, note) => {
            val (newSubIO, subTransBuilder) = simplifiedIOs(circuitptr)
            val newSubRef = oldSubRef.copy(ioType = newSubIO)
            subTransBuilder(oldSubRef, newSubRef).foreach({case (oldPath, newRef) =>
              path2ref += oldPath -> newRef
            })
            SubModuleDecl(newSubRef, circuitptr, note)
          }

          case _ => super.transform(cmd)
        }

        def makePath(expr: ExprHW): Option[IOPathTrace] = expr match {
          case RefIO(oldref,_) => Some(IPTStart(oldref))
          
          case RefVIndex(source, index, _)  => makePath(source).map(srcPath => IPTSelectOne(srcPath, index))
          case RefTLookup(source, field, _) => makePath(source).map(srcPath => IPTField(srcPath, field))
          
          case _ => None
        }
        override def transform(ref: RefHW): RefHW  = ref match {
          case RefVIndex(_,_,_) | RefTLookup(_,_,_) =>
            makePath(ref).flatMap(path => path2ref.get(path)).getOrElse(ref)

          case _ => super.transform(ref)
        }
      }
      ElaboratedModule(selftype, newIO, Transformer.transform(oldBody))
    }})

    ElaboratedCircuit(updatedModules)
  }
}
