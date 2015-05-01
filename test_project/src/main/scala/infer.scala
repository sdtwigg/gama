package test

import gama.api._
import gama.library._

@module class InferModule extends Module(Output(UInt(16))) {
  val myKnownReg = Reg(UInt(4))
  myKnownReg := 2.U

  val myRegU = Reg(UInt())
  myRegU := myKnownReg

  val myOp = myRegU + 1.U + 1.U

  val myRegVU = Reg(Vec(4, UInt()))
  
  val wp = myRegVU(0.U)
  wp := 1.U

  val rp = myRegVU(1.U)

  val pos0 = myRegVU(0)
  val myWire = Wire(UInt())
  myWire := pos0

  myRegVU(1) := 4.U

  val myOp2 = myRegVU(0) + 1.U
  myWire := Mux(True, myRegVU, myRegVU)(0)

  val myOp3 = (2.U * 2.U)
  val myOp4 = (2.U << 2.U)
}
