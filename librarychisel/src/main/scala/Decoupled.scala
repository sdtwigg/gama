package gama
package library

import gama._
import gama.api._

final class Decoupled[+T<:Data] private (model: T) extends HardwareTuple with BundleReflection {
  val valid = Output(Bool())
  val bits  = Output(model.copy)
  val ready =  Input(Bool())
  def fire(implicit em: EnclosingModule): Bool = valid && ready

  def simplecopy: this.type = new Decoupled(model).asInstanceOf[this.type]
}
object Decoupled {
  def apply[T<:Data](model: T): Decoupled[T] = new Decoupled(model)
 
  import gama.internal._
  implicit def selfMuxer[D<:Data: Muxable]: Muxable[Decoupled[D]] = new Muxable[Decoupled[D]] {
    def muxRetVal(tc: Decoupled[D], fc: Decoupled[D]) = {
      Decoupled(implicitly[Muxable[D]].muxRetVal(tc.bits, fc.bits))
    }
  }
}

