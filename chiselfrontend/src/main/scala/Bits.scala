package gama
import internal._

abstract class Bits(initialNode: Node) extends Element(initialNode) {
  def extract(position: Int)(implicit em: EnclosingModule): Bool = ExtractOp.Bool(this, position, em)
  def extract(left_pos: Int, right_pos: Int)(implicit em: EnclosingModule): UInt = ExtractOp.UInt(this, left_pos, right_pos, em)

  def apply(position: Int, em: EnclosingModule): Bool = extract(position)(em)
  def apply(left_pos: Int, right_pos: Int, em: EnclosingModule): UInt = extract(left_pos, right_pos)(em)

  import scala.language.experimental.macros
  def apply(arg0: Int): Bool = macro macroDef.transformapply1 
  def apply(arg0: Int, arg1: Int): UInt = macro macroDef.transformapply2
  
}
object Bits {
/*
// Making this available would allow muxing between UInt and SInt (and thus making a Vec of them)
  implicit object selfmuxer extends SelfMuxable[Bits] {
    def mux(tc: Bits, fc: Bits) = ???
  }
// These two allow muxing but not Vec creation
  implicit object usintmuxer1 extends Muxable[UInt, SInt] {
    def mux[D<:Data, A >: UInt <: D, B >: SInt <: D](tc: UInt, fc: SInt): D = ???.asInstanceOf[D]
  }
  implicit object usintmuxer2 extends Muxable[SInt, UInt] {
    def mux[D<:Data, A >: SInt <: D, B >: UInt <: D](tc: SInt, fc: IInt): D = ???.asInstanceOf[D]
  }
*/
}
