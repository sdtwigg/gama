package test

import gama.frontend.api._
import gama.library._

@module class SandboxModule extends Module(new Bundle with Anon {
  val in_uint  = Input(UInt(4))
  val in_bool  = Input(Bool())
  val out_uint = Output(UInt(4))
}) {

  val myVVB1 = Reg(Vec(2, Vec(2, new MyBundle())))
  val myVVB2 = Reg(Vec(2, Vec(2, new MyBundle())))
  val myVVB3 = Reg(Vec(2, Vec(2, new MyBundle())))

  io.out_uint := myVVB1.asUInt

  myVVB2.connectFromUInt(io.in_uint)
  myVVB3 := myVVB3.fromUInt(io.in_uint)

  val myOp = Mux(io.in_bool, myVVB2, myVVB3)
  io.out_uint := Cat(io.in_uint, io.in_bool)
  io.out_uint := Fill(4, io.in_uint)
}

