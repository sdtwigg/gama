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

  val myb = Wire(new MyBundle)
  val tricky = Mux(True, myb, myb).a + 1.U

  val test2 = AsHWConstant(test)
  val test3 = AsHWConstant(U(1,4))
  val test4 = AsHWConstant(S(1,4))
  val test5 = AsHWConstant(1.U)
  val test6 = AsHWConstant(1.S)

  val vtest = AsHWConstant(LitVec.SW( (3,3), (4,4), (0,1), (5,5), (1,2) ))
  val mycalias = vtest(notuint)
  val vecreg = Reg(Vec(4,UInt()))
  val myalias = vecreg(notuint)
  myalias := 1.U
  
  import gama.internal.{LitMap, VecLitMap, SIntLitMap}
  val ivec11 = VecLitMap(Seq(
    SIntLitMap(1,2), SIntLitMap(3,3), SIntLitMap(5,5), SIntLitMap(4,4)
  ))
  val ivec12 = VecLitMap(Seq(
    SIntLitMap(10,10), SIntLitMap(13,13), SIntLitMap(16,16), SIntLitMap(17,17)
  ))
  val ivec13 = VecLitMap(Seq(
    SIntLitMap(20,20), SIntLitMap(23,13), SIntLitMap(26,26), SIntLitMap(27,27)
  ))
  val vecvec1 = VecLitMap(Seq(ivec11, ivec12, ivec13))
  
  val ivec21 = VecLitMap(Seq(
    SIntLitMap(30,30), SIntLitMap(33,33), SIntLitMap(35,35), SIntLitMap(34,34)
  ))
  val ivec22 = VecLitMap(Seq(
    SIntLitMap(40,40), SIntLitMap(43,43), SIntLitMap(46,46), SIntLitMap(47,47)
  ))
  val ivec23 = VecLitMap(Seq(
    SIntLitMap(50,50), SIntLitMap(53,53), SIntLitMap(56,56), SIntLitMap(57,57)
  ))
  val vecvec2 = VecLitMap(Seq(ivec21, ivec22, ivec23))
  val vecvecvec = VecLitMap(Seq(vecvec1, vecvec2))
  val vecvecveclit = AsHWConstant(vecvecvec.manifest)
 
  // Test empty literal vecs
  val zeroveclit = AsHWConstant(LitVec.S(Seq.empty))

  val zerovecvec = VecLitMap.apply[Vec[SInt],VecLitMap[SInt,SIntLitMap]](Seq.empty)
  val zerovecveclit = AsHWConstant(zerovecvec.manifest)
  
  type VecOf[D<:Data,DLM<:LitMap[D]] = VecLitMap[D, DLM]
  type VecOfSInt = VecOf[SInt,SIntLitMap]
  type VecVecOfSInt = VecOf[Vec[SInt], VecOfSInt]
  type VecVecVecOfSInt = VecOf[Vec[Vec[SInt]], VecVecOfSInt]
  val zerovecvecvec = VecLitMap(Seq.empty[VecVecOfSInt])
  val zerovecvecveclit = AsHWConstant(zerovecvecvec.manifest)
  val zerovecvecvecvec = VecLitMap(Seq.empty[VecVecVecOfSInt])
  val zerovecvecvecveclit = AsHWConstant(zerovecvecvecvec.manifest)
}
