package test

import gama.frontend.api._
import gama.library._

@module class SandboxModule extends Module(new Bundle with Anon {
  val in_uint  = Input(UInt(4))
  val in_bool  = Input(Bool())
  val out_uint = Output(UInt(4))
}) {

  val myVVB1 = Reg(Vec(4, Vec(4, new MyBundle())))
  val myVVB2 = Reg(Vec(4, Vec(4, new MyBundle())))

  io.out_uint := Mux(True, Mux(True, myVVB1, myVVB2), Mux(False, myVVB2, myVVB1))(0)(0).a
}

