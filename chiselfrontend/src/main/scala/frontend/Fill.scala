package gama
package frontend

object Fill {
  import scala.language.experimental.macros
  import implementation.macrodefs.{TransformMacro => XFORM}
  
  // External API
  def apply(n: Int, mod: UIntLike): UInt = macro XFORM.doFill.twoarg

  // external->internal API
  def doFill(n: Int, mod: UIntLike, info: EnclosureInfo): UInt = n match {
    case _ if n < 0 => throw new IllegalArgumentException("For Fill, n must be non-negative integer")
    case 0 => LiteralUInt(0,0)
    case _ if n > 0 => Cat.doCat(Seq.fill(n)(mod), info)
  }
}
