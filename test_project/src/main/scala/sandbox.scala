package test

import gama.frontend.api._
import gama.library._

@module class SandboxModule extends Module(new Bundle with Anon {
  val in_uint  = Input(UInt(4))
  val in_bool  = Input(Bool())
  val out_vuint = Output(Vec(2,UInt(8)))
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
  
  io.out_vuint(io.in_uint) <> io.in_uint
  io.out_uint(3,0)(2,1) <> io.in_uint(4,3)(1,1)(0,0)

  io.out_vuint(io.out_vuint(io.in_uint)) := 0.U
  io.out_uint := io.out_vuint(io.out_vuint(io.in_uint))
}


@module class Sandbox2Module extends Module(new Bundle with Anon {
  val in_uint  = Input(UInt(4))
}) {
  val mySInt = Reg(SInt(4))
  
  mySInt := 0.U
  when(io.in_uint(0)) {
    mySInt := 1.U
    val myInner = Wire(UInt(2))
    when(io.in_uint(1)) {
      mySInt := 2.U
      mySInt := 3.U
      myInner := 10.U
    }.otherwise {
      mySInt := 4.U
      myInner := 11.U
    }
    mySInt := 99.U
  }.otherwise {
    //mySInt := 5.U
    when(io.in_uint(1)) {
      mySInt := 6.U
    }.otherwise {
      mySInt := 7.U
      mySInt := 8.U
    }
  }

  val extrTest = Wire(SInt(8))
  extrTest := 1.S
  when(io.in_uint(0)) {
    extrTest(2,0) := 0.U
  }.otherwise {
    extrTest(5,3) := 0.U
  }
}

