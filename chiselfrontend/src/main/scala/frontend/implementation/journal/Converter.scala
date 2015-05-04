package gama
package frontend
package implementation
package journal

import intermediate._

object Converter {
  def apply(module: Module[_<:Data]): ElaboratedModule = {
    val reftable = new RefTable(None)
    val selfRef = ToFIR.processIO(module, (thw) => ModuleThis(thw), reftable)
    val body = ToFIR.convertJournal(module.getActiveJournal, Some(reftable), None, None, None)
    ElaboratedModule(selfRef.ioType, body)
  }
}
