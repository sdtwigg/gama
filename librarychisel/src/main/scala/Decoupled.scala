package gama
package library

import gama._
import gama.api._

final class Decoupled[+T<:Data] private (model: T) extends HardwareTuple with BundleReflection {
  val valid = Output(Bool())
  val bits  = Output(model.copy)
  val ready =  Input(Bool())
  def fire(implicit em: EnclosingModule): Bool = valid && ready

  def copy: this.type = new Decoupled(model).asInstanceOf[this.type]
}
object Decoupled {
  def apply[T<:Data](model: T): Decoupled[T] = new Decoupled(model)
}

