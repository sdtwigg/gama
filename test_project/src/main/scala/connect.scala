package test

import gama.frontend.api._
import gama.library._

@bundle class BiBundle extends Bundle {
  val a = Input(  UInt(width=8) )
  val b = Output( UInt(width=8) )
}

@module class BiConnectModule extends Module(Vec(4, new BiBundle)) {
  val wire = Wire(Vec(4, new BiBundle))
  wire <> io
}
