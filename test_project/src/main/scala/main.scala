package test
import gama._
import gama.api._
import gama.annotations._

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

@bundle @probe final class Decoupled[+T<:Data] private (model: T) extends Bundle {
  val valid = Output(Bool())
  val bits  = Output(model.copy)
  val ready = Input( Bool())
  def fire(implicit em: EnclosingModule): Bool = valid && ready
}
object Decoupled {
  def apply[T<:Data](model: T): Decoupled[T] = new Decoupled(model)
}

@bundle class DecoupledExample extends Bundle {
  val in  = Flipped(Decoupled(new MyBundle))
  val out =         Decoupled(new MyBundle)
}

@bundle @probe class MyChildBundle extends MyBundle {
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
}

@module class InnerModule extends Module(new Nested2 with Anon {
  val in1 = Input(UInt(4))
  val out = Output(UInt(4))
}) {
  val uint = Reg(UInt(2))
  uint := io.in1 * U(10,16) + 7.U * 4.U + True
  io.out := ( Wire(Vec(4,UInt())) ).lookup(uint)

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
@module @probe class OtherModule extends Module(new DecoupledExample) {
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

@module class ExampleModule protected () extends Module(new ExampleIO) {
  println(this)
  println(ExampleModule.this)

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
  
  val myInnerModule = Module(new InnerModule)
  val myOtherModule = Module(new OtherModule)

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
