package test

import gama.frontend.api._
import gama.library._

@module class RegModule extends Module(new Bundle with Anon {
  val in_uint  = Input(UInt(4))
  val in_bool  = Input(Bool())
  val out_uint = Output(UInt(4))
}) {
  val myRegm__ = Reg(io.in_uint)
  val myRegi__ = Reg(RInit(0.U))
  val myRegim_ = Reg(RInitModel(0.U))
  val myRegm_i = Reg(io.in_uint, RInit(0.U))
  myRegm__ := io.in_uint
  myRegi__ := io.in_uint
  myRegim_ := io.in_uint
  myRegm_i := io.in_uint
  
  val myrRegi__ = Reg(RInit(0.U, io.in_bool))
  val myrRegim_ = Reg(RInitModel(0.U, io.in_bool))
  val myrRegm_i = Reg(io.in_uint, RInit(0.U, io.in_bool))
  myrRegi__ := io.in_uint
  myrRegim_ := io.in_uint
  myrRegm_i := io.in_uint
}

