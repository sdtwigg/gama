package test

import gama._
import gama.api._
import gama.library._

object testmain {
  def main(args: Array[String]) {

    val myTopModule = ExampleModule()
    val myReader = gama.internal.reader.FoldedJournalReader.Colorful

    println(myReader.parseCircuit(myTopModule) mkString("\n"))
  }
}
/*
// These fails to compile as desired
@bundle trait TestTrait extends Bundle{
  val test = UInt()
}
@bundle abstract class TestAbstractClass extends Bundle{
  val test = UInt()
}
*/

@bundle class ExampleIO extends Bundle {
  val in1 = Input( UInt(width=8))
  val in2 = Input( new MyChildBundle)
  val out = Output(UInt(width=8))
}

@bundle class MyBundle extends Bundle {
  val a = UInt(width=8)
  val b = UInt(width=8)
}

@bundle class DecoupledExample extends Bundle {
  val in  = Flipped(Decoupled(new MyBundle))
  val out =         Decoupled(new MyBundle)
}

@bundle class MyChildBundle extends MyBundle {
  var unstable = 5
  val stable = 7
  def bleh = c
  val c = UInt(width=8)
}

trait Nested extends Bundle {
  val a = Input(UInt(width=8))
  val b = Input(UInt(width=8))
}

trait Nested2 extends Nested {
  var unstable = 5
  val stable = 7
  def bleh = c
  val c = Output(UInt(width=8))
  val d = Output(UInt(width=8))
}

@module class InnerModule extends Module(new Nested2 with Anon {
  val in1 = Input(UInt(4))
  val out = Output(UInt(4))
}) {
  val uint = Reg(UInt(2))
  uint := io.in1 * U(10,16) + 7.U * 4.U + True
  io.out := ( Wire(Vec(4,UInt())) ).lookup(uint)

  ExecBlock {
    val test_u2s: SInt = 1.U.toSInt
    val test_u2u: UInt = 1.U.toUInt
    val test_uAs: SInt = 1.U.asSInt
    val test_uAu: UInt = 1.U.asUInt
    
    val test_s2s: SInt = 1.S.toSInt
    val test_s2u: UInt = 1.S.toUInt
    val test_sAs: SInt = 1.S.asSInt
    val test_sAu: UInt = 1.S.asUInt
    
    val test_b2s: SInt = True.toSInt
    val test_b2u: UInt = True.toUInt
    val test_bAs: SInt = True.asSInt
    val test_bAu: UInt = True.asUInt
  }

  val sint = Reg(SInt())
  ExecBlock {
    val test_upu = uint + uint
    val test_ups = uint + sint
    val test_spu = sint + uint
    val test_sps = sint + sint
   
    val ouint: BaseElem = uint
    val osint: BaseElem = sint
    val otest_upu = ouint + ouint
    val otest_ups = ouint + osint
    val otest_spu = osint + ouint
    val otest_sps = osint + osint
  }

  val andR = uint.andR
  val orR  = uint.orR
  val xorR = uint.xorR
}
@module class OtherModule extends Module(new DecoupledExample) {
  val uint = Reg(UInt(2))

  val myBundle = Wire(new MyBundle)
  val myChildBundle = Wire(new MyChildBundle)
  val mymux = Mux(True, myChildBundle, myBundle)

  myBundle := myChildBundle
  myChildBundle := myBundle
  
  val myBundleVec      = Wire(Vec(2, new MyBundle))
  val myChildBundleVec = Wire(Vec(2, new MyChildBundle))
  val vecmux = Mux(True, myBundleVec, myChildBundleVec.asInstanceOf[Vec[MyBundle]])
    // type cast IS required here since Vec's type is invariant
  
  val myBundleVecVec   = Wire(Vec(2,Vec(2, new MyBundle)))

  //uint := io.in1
  //io.out := ( Wire(Vec(4,UInt())) ).lookup(uint)
}

@module class MemModule extends Module(new DecoupledExample) {
  val uint = Reg(UInt(2))
  val myreg = Reg(new MyChildBundle)

  val myMem = Mem(new MyBundle, 16)
  myMem(uint) := myMem(uint + 1.U)
  
  when(True) {
    val memread = myMem(0.U)
    val memwrite = myMem(2.U)
    memwrite := memread
    myMem.write(True, memread)
  }

  myMem(myMem(uint).a).b := uint * 2.U
  myMem.write(False, myreg)
}

@module class ConnectModule extends Module(new DecoupledExample) {
  val uint = Reg(UInt())
  val sint = Reg(SInt())
  val bool = Reg(Bool())
  uint := uint
  sint := sint
  bool := bool
  
  val uintVec = Reg(Vec(2,UInt()))
  val sintVec = Reg(Vec(2,SInt()))
  val boolVec = Reg(Vec(2,Bool()))

  uintVec := uintVec
  sintVec := sintVec
  boolVec := boolVec

  sintVec := uintVec
  uintVec := sintVec
  sintVec := boolVec
  uintVec := boolVec
  
  // These do not compile, as desired
  //boolVec := uintVec
  //boolVec := sintVec

  val myPVec = Reg(Vec(2,new MyBundle))
  val myCVec = Reg(Vec(2,new MyChildBundle))
  myPVec := myCVec
  myCVec := myPVec // these work as designed, somewhat unfortunately

  val freeLitVec = LitVec.U(0,1,2,3,4,5)
  val enclLitVec = AsHWConstant(LitVec.U(Seq(10,11,12,13,14,15)))

  val muxLitVec = Mux(bool, freeLitVec, enclLitVec)
  val lituint1a = AsHWConstant(freeLitVec(1))
  val lituint2a = AsHWConstant(enclLitVec(2))
  val lituint1b = freeLitVec(uint)
  val lituint2b = enclLitVec(uint)
}

@module class ExtractModule extends Module(new DecoupledExample) {
  val uintWire = Wire(UInt())
  uintWire(1) := True
  val vecReg = Reg(Vec(4,UInt()))
  vecReg(0.U)(3,0) := 4.U
  vecReg(1)(3,0) := (uintWire + 1.U).extract(3,0)
  val uintOp = uintWire + uintWire
  //uintOp(1) := True // throws exception, as desired

  val myMem = Mem(UInt(), 16)
  myMem(0.U)(15,0) := 0.U // not sure if this should be allowed

  val myB = Reg(new MyBundle)
  myB.a(4,3) := myB.b(0).asSInt

  // this should probably get folded somehow? if possible
  uintWire(15,0)(7,4)(0) := False
}

@module class CopyModule extends Module(new Bundle with Anon {
  
  val t1_in_uint  = Input( UInt()).copy
  val t1_out_uint = Output(UInt()).copy

  val t2_in_vecuint  = Vec(4, Input( UInt()) )
  val t2_out_vecuint = Vec(4, Output(UInt()) )

  val t3_bundle1 = Decoupled(UInt())
  val t3_bundle2 = Flipped(Decoupled(UInt()))
  val t3_bundle3 = t3_bundle2.copy

  val t4_vecvec1 = Vec(4, Vec(4, Input(new MyBundle())))
  val t4_vecvec2 = Vec(4, Decoupled(Vec(4, new MyBundle).copy))
  val t4_vecvec3 = t4_vecvec2.copy.copy.copy
}) {
  io.t2_out_vecuint(1) := io.t2_in_vecuint(0)
}

@module class ExampleModule protected () extends Module(new ExampleIO) {
  val data_width = 32
  val vec_length = 4
  val dtype_uint = UInt(data_width)
  val vectype = Vec(vec_length, dtype_uint)
  val exampleVec   = Wire(vectype)
  val example2Vec  = Reg(vectype)

  val uint1 = Wire(UInt(8))
  val uint2 = Wire(dtype_uint)
  val select1 = Wire(Bool())
  val select2 = Wire(Bool())
  val select3 = Wire(Bool())
  val waste = select3
  //val wrong = vectype(uint1)
  val sint1 = Reg(SInt())
  sint1 := -1.S

  val myNewVec  = Wire(Vec(2, UInt(4)))

  val myUnknownVec = Wire(Vec(2, UInt()))
  myUnknownVec(0) := uint1 pad myUnknownVec(0)
  myUnknownVec(1) := uint2 pad myUnknownVec(1)

  val myMux    = Mux(select1, uint1, myNewVec(1))
  val myVecMux = Mux(select1, myNewVec, myUnknownVec)
  val myVecMux2 = Mux(select1, myNewVec, myVecMux)
  val test = Reg(UInt())
  test := io.in1
  io.out := test
  
  test := select1
  
  val myAccessor = myNewVec(uint2)
  test := myAccessor
  myAccessor := uint2

  test := myNewVec(uint2)
  
  test := select1
  test := U(15, 14) + 1.U + 1.U
  test := uint1 + uint2 + uint1 + 1.U
  test := uint1(1,2) + uint1(1) + uint1(2,3)
  val added = uint1 + uint2
  test := added
  
  val myInnerModule   = Module(new InnerModule)
  val myOtherModule   = Module(new OtherModule)
  val myMemModule     = Module(new MemModule)
  val myConnectModule = Module(new ConnectModule)
  val myExtractModule = Module(new ExtractModule)
  val mCopyModule     = Module(new CopyModule)

  myInnerModule.io.in1 := test
  test := myInnerModule.io.out
  
  when(select1) {
    when(select2) {
      test := select1
    }
    val myInternalMath = uint1 * uint2
    test := myInternalMath
  }.elsewhen(True && (test !== select1) && (uint1 !== 10.U) && (uint2 > uint1)) {
    test := select1
  }.otherwise {
    test := uint2
  }

  val pos = Wire(UInt())
  val wireVecVec = Wire(Vec(5,Vec(5,UInt(6))))
  val lookup = wireVecVec(pos)(pos)(3,2)(1,0)(0)
  val wireVecVec2 = Wire(Vec(5,Vec(5,UInt())))
  val test1: Bool = wireVecVec2(test)(test)(5,1)(0)
  Reg(UInt()) := test + Reg(UInt())

  val myBool = Wire(Bool())
  val myBReg  = Reg(Bool()) := myBool && True || myBool || myBool ^ !myBool 
 
  val switchReg = Reg(UInt())
  switch(switchReg)(Seq(
    is(uint1){
      test := 0.U
    },
    is(uint2){
      test := 1.U
    }
  ))
}
object ExampleModule {
  def apply(): ExampleModule = Module(new ExampleModule)
}

@module class TestModule protected () extends Module(Output(UInt(32))) {
  import scala.reflect.runtime.universe._
  def getTypeT[A: TypeTag](in: A) = typeTag[A]

  val myUInt = UInt()
  val myUWire = Wire(myUInt)
  val myUReg = Reg(myUInt)
  val myUMux = Mux(Reg(Bool()), myUWire, myUReg)

  println(getTypeT(myUInt))
  println(getTypeT(myUWire))
  println(getTypeT(myUReg))
  println(getTypeT(myUMux))
  println(getTypeT(myUMux.node))
  
  val mySInt = SInt(32)
  println(getTypeT(mySInt))

  val myUVec  = Wire(Vec(3, myUInt))
  val myUVec2 = Wire(Vec(3, myUInt))
  println(getTypeT(myUVec))

  myUVec := myUVec
  val myUVecMux = Mux(Reg(Bool()), myUVec, myUVec)
  val myUVecReg = Reg(myUVec)
  println(getTypeT(myUVecMux))
  println(getTypeT(myUVecReg))

  val myBool = Bool()

}
object TestModule {
  def apply() = Module(new TestModule)
}
