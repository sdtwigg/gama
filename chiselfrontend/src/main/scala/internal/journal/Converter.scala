package gama
package internal
package journal

import frontend._

case class ElaboratedModule(io: TypeHW, body: BlockHW)

object Converter {
  def apply(module: Module[_<:Data]): ElaboratedModule = {
    val reftable = new RefTable(None)
    val selfRef = ModuleThis(ToFIR.constructType(module.io))
    val ioSymbol = reftable.add(module.io, (RefIO(selfRef), true))
    val body = ToFIR.convertJournal(module.getActiveJournal, Some(reftable), None, None, None)
    ElaboratedModule(selfRef.ioType, body)
  }
}

