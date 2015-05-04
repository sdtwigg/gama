package gama
package frontend
package api

trait NodeAPI {
  // The actual Node transforms
  val Reg = gama.frontend.Reg
  val Wire = gama.frontend.Wire
  val Mux = gama.frontend.Mux
  val Mem = gama.frontend.Mem
  val AsHWConstant = gama.frontend.AsHWConstant

  // Case Classes for specializing the Reg Transform
  val RModel = gama.frontend.RModel
  val RInit = gama.frontend.RInit
  val RInitModel = gama.frontend.RInitModel
}
