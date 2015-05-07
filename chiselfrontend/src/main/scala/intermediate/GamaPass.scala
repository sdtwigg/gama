package gama
package intermediate

trait GamaPass {
  def transform(target: ElaboratedModule): ElaboratedModule
}

abstract class PassMerger(passes: Iterable[GamaPass]) extends GamaPass{
  def transform(target: ElaboratedModule): ElaboratedModule = 
    passes.foldLeft(target)((target, pass) => pass.transform(target))
}
