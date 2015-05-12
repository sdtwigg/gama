package gama
package intermediate

trait GamaPass {
  val name: String
  def transform(target: ElaboratedModule): ElaboratedModule
  def passNote: GamaNote = GamaNote(GTSourcePass(name))
}

abstract class PassMerger(passes: Iterable[GamaPass]) extends GamaPass{
  val name = this.toString
  def transform(target: ElaboratedModule): ElaboratedModule = 
    passes.foldLeft(target)((target, pass) => pass.transform(target))
}
