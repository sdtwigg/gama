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
  val RModel = gama.frontend.Reg.RModel
  val RInit = gama.frontend.Reg.RInit
  val RInitModel = gama.frontend.Reg.RInitModel

  // Some other things
  val Cat  = gama.frontend.Cat
  val Fill = gama.frontend.Fill
}
