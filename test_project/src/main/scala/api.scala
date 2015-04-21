package test

import gama.api._
import gama.library._

@module class APIModule extends Module(Output(UInt())) {
  val uint1 = Wire(UInt())
  val uint2 = Reg(UInt())

  val cat = uint1 ## uint2

  val notuint: UInt = ~uint1
  val notbool: Bool = ~uint1(0)
  val notuintlike1: UIntLike = ~uint1
  val notuintlike2: UIntLike = ~(uint1: UIntLike)
  val notuintlike3: UIntLike = ~notbool

  val notdigital1: Digital = ~notuint
  val notdigital2: Digital = ~(notuint: Digital)

  val extendedtest = ~(-((((uint1 ## uint2) - (uint1)) << 2.U)(7,4)(2,1)(1)))
  val part2 = ((extendedtest >> 1.U) * 2.U / 3.U % 5.U).andR
  val extendedtest2 = ((~(-((((uint1 ## uint2) - (uint1)) << 2.U)(7,4)(2,1)(1))) >> 1.U) * 2.U / 3.U % 5.U).andR 
}
