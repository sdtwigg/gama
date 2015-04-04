package test
import gama._
import gama.macros._

object testmain {
  def main(args: Array[String]) {

    val myTestModule = ExampleModule()
    val myReader = gama.internal.FoldedJournalReader.Colorful
    println(myReader.parseModule(myTestModule))
    println(myReader.parseModule(myTestModule.myInnerModule))
  }
}

class ExampleIO extends HardwareTuple {
  val in  = UInt(width=8)
  val out = UInt(width=8)

  lazy val subfields = Vector(
    ("in", in),
    ("out", out)
  )
  def copy = (new ExampleIO).asInstanceOf[this.type]
}

@module class ExampleModule protected () extends Module(new ExampleIO) {
  val dtype_uint = UInt()

  val uint1 = Wire(UInt(8))
  val uint2 = Wire(dtype_uint)
  val select1 = Wire(Bool())
  val select2 = Wire(Bool())
  val select3 = Wire(Bool())
  val waste = select3

  val myNewVec = Wire(Vec(2, UInt(4)))

  val myMux = Mux(select1, uint1, myNewVec(1))
  val test = Reg(UInt())
  test := io.in
  io.out := test
  
  val myAccessor = myNewVec(uint2)
  test := myAccessor

  test := myNewVec(uint2)
  
  test := select1
  test := uint1 + uint2 + uint1
  test := uint1(1,2) + uint1(1) + uint1(2,3)
  val added = uint1 + uint2
  test := added

  class InnerModule extends Module(new ExampleIO) {
    val uint = Reg(UInt(2))
    uint := io.in
    io.out := ( Wire(Vec(4,UInt())) ).lookup(uint)
  }

  val myInnerModule = Module(new InnerModule)

  myInnerModule.io.in := test
  test := myInnerModule.io.out
  
  when(select1) {
    when(select2) {
      test := select1
    }
    test := uint1
  }.elsewhen({Reg(Bool()) := select1}) {
    test := select2
  }.otherwise {
    test := uint2
  }
}
object ExampleModule {
  def apply(): ExampleModule = Module(new ExampleModule)
}

class TestModule protected () extends Module(UInt()) {
  import scala.reflect.runtime.universe._
  def getTypeT[A: TypeTag](in: A) = typeTag[A]

  val myUInt = UInt()
  val myUWire = Wire(myUInt)
  val myUReg = Reg(myUInt)
  val myUMux = Mux(Bool(), myUWire, myUReg)

  println(getTypeT(myUInt))
  println(getTypeT(myUWire))
  println(getTypeT(myUReg))
  println(getTypeT(myUMux))
  println(getTypeT(myUMux.node))
  
  val mySInt = SInt(32)
  println(getTypeT(mySInt))

  val myUVec = Vec(3, myUInt)
  val myUVec2 = Vec(3, myUInt)
  println(getTypeT(myUVec))

  myUVec := myUVec
  val myUVecMux = Mux(Bool(), myUVec, myUVec)
  val myUVecReg = Reg(myUVec)
  println(getTypeT(myUVecMux))
  println(getTypeT(myUVecReg))

  val myBool = Bool()
}
object TestModule {
  def apply() = Module(new TestModule)
}
