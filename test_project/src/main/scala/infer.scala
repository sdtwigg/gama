package test

import gama.frontend.api._
import gama.library._

@bundle class UBundle extends Bundle {
  val a = UInt()
  val b = UInt()
  val c = UInt(width=8)
}

@module class InferModule extends Module(new ExampleIO) {
  val myKnownReg = Reg(UInt(4), RInit(0.U))
  myKnownReg := 2.U

  val myRegU = Reg(UInt())
  myRegU := myKnownReg

  val myOp = myRegU + 1.U + 1.U

  val myRegVU = Reg(Vec(8, UInt()))
  
  val wp = myRegVU(myKnownReg)
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
  val myOp5 = (2.U << 2)
  val myOp6 = (2.U >> 2.U)
  val myOp7 = (2.U >> 2)

  val myUB = Reg(new UBundle)
  myUB.a := myWire
  myUB.b := myUB.a

  val myUBVV = Reg(Vec(4,Vec(4,new UBundle)))
  val myWire2 = Mux(False, Mux(True, myUBVV(0), myUBVV(1))(2), myUBVV(2)(2))
  val myAcc = myUBVV(myRegU)(myRegU + 1.U)
  myAcc := myUB

  val ioWire = Wire(new UBundle)
  io.in2 <> ioWire

  val mySubModule = Module(new SubModule(defaultReset=myWire(0)))
  val subWire1 = Wire(UInt())
  subWire1 <> mySubModule.io.out

  val mySInt = Wire(SInt())
  mySInt := 2.U

  // This circular reference should fail inference
  val myWirea = Wire(UInt())
  val myWireb = Wire(UInt())
  val myWirec = Wire(UInt())
  myWirea := myWireb
  myWireb := myWirec
  myWirec := myWirea
}
