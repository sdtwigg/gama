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
  val test = uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1 + uint1

  val test2 = AsHWConstant(test)
  val test3 = AsHWConstant(U(1,1))
  val test4 = AsHWConstant(S(1,1))
  val test5 = AsHWConstant(1.U)
  val test6 = AsHWConstant(1.S)

  val vtest = AsHWConstant(LitVec.SW( (3,3), (4,4), (0,1), (5,5), (1,1) ))
  
  import gama.internal.{LitMap, VecLitMap, SIntLitMap}
  val ivec11 = VecLitMap(Seq(
    SIntLitMap(1,Some(1)), SIntLitMap(3,Some(3)), SIntLitMap(5,Some(5)), SIntLitMap(4,Some(4))
  ))
  val ivec12 = VecLitMap(Seq(
    SIntLitMap(10,Some(10)), SIntLitMap(13,Some(13)), SIntLitMap(16,Some(16)), SIntLitMap(17,Some(17))
  ))
  val ivec13 = VecLitMap(Seq(
    SIntLitMap(20,Some(20)), SIntLitMap(23,Some(13)), SIntLitMap(26,Some(26)), SIntLitMap(27,Some(27))
  ))
  val vecvec1 = VecLitMap(Seq(ivec11, ivec12, ivec13))
  
  val ivec21 = VecLitMap(Seq(
    SIntLitMap(30,Some(30)), SIntLitMap(33,Some(33)), SIntLitMap(35,Some(35)), SIntLitMap(34,Some(34))
  ))
  val ivec22 = VecLitMap(Seq(
    SIntLitMap(40,Some(40)), SIntLitMap(43,Some(43)), SIntLitMap(46,Some(46)), SIntLitMap(47,Some(47))
  ))
  val ivec23 = VecLitMap(Seq(
    SIntLitMap(50,Some(50)), SIntLitMap(53,Some(53)), SIntLitMap(56,Some(56)), SIntLitMap(57,Some(57))
  ))
  val vecvec2 = VecLitMap(Seq(ivec21, ivec22, ivec23))
  val vecvecvec = VecLitMap(Seq(vecvec1, vecvec2))
  val vecvecveclit = AsHWConstant(gama.LitMapToData(vecvecvec))
 
  // Test empty literal vecs
  val zeroveclit = AsHWConstant(LitVec.S(Seq.empty))

  val zerovecvec = VecLitMap.apply[Vec[SInt],VecLitMap[SInt,SIntLitMap]](Seq.empty)
  val zerovecveclit = AsHWConstant(gama.LitMapToData(zerovecvec))
  
  type VecOf[D<:Data,DLM<:LitMap[D]] = VecLitMap[D, DLM]
  type VecOfSInt = VecOf[SInt,SIntLitMap]
  type VecVecOfSInt = VecOf[Vec[SInt], VecOfSInt]
  type VecVecVecOfSInt = VecOf[Vec[Vec[SInt]], VecVecOfSInt]
  val zerovecvecvec = VecLitMap(Seq.empty[VecVecOfSInt])
  val zerovecvecveclit = AsHWConstant(gama.LitMapToData(zerovecvecvec))
  val zerovecvecvecvec = VecLitMap(Seq.empty[VecVecVecOfSInt])
  val zerovecvecvecveclit = AsHWConstant(gama.LitMapToData(zerovecvecvecvec))
}
