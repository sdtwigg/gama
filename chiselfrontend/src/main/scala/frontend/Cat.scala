package gama
package frontend

object Cat {
  import scala.language.experimental.macros
  import implementation.macrodefs.{TransformMacro => XFORM}
  
  // External API
  def apply(term: Data, terms: Data*): UInt = macro XFORM.doCatObj.multiarg
  def apply(terms: Iterable[Data]): UInt = macro XFORM.doCatObj.seqarg

  // external->internal API
  def doCat(terms: Iterable[Data], info: EnclosureInfo): UInt = {
    if(terms.size>0) terms.map(_.do_asUInt(info)).reduceLeft((left, right) => left.do_cat(right, info))
    else LiteralUInt(0,0)
  }
}
