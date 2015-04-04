package test
import gama._
import gama.macros._

object testmain {
  def main(args: Array[String]) {

    val myTestModule = ExampleModule()
    val myReader = gama.internal.FoldedJournalReader
    println(myReader.Colorful(myTestModule.getActiveJournal))
    println(myReader.Colorful(myTestModule.myInnerModule.getActiveJournal))

/*  // These do not compile, as desired
    val myBMux = Mux(myUInt, mySInt)
    println(getTypeT(myBMux))
    val myBVec = new Vec(Vector(myUInt, myUInt, mySInt)) // doesn't compile as desired
*/
  }
}

@module class ExampleModule protected () extends Module(UInt()) {
  val uint1 = Wire(UInt(8))
  val uint2 = Wire(UInt())
  val select1 = Wire(Bool())
  val select2 = Wire(Bool())
  val select3 = Wire(Bool())

  val myNewVec = Wire(Vec(2, UInt(4)))
  //val myOldVec = Vec(Vector(uint1,myNewVec.elements(1),Wire(UInt())))

  val myMux = Mux(select1, uint1, myNewVec(1))
  val test = Reg(UInt())
  
  val myAccessor = myNewVec(uint2)
  test := myAccessor

  test := myNewVec(uint2)
  
  test := select1
  test := uint1 + uint2 + uint1
  test := uint1(1,2) + uint1(1) + uint1(2,3)
  val added = uint1 + uint2
  test := added

  class InnerModule extends Module(UInt()) {
    val uint = Wire(UInt())
    uint := io
  }

  val myInnerModule = Module(new InnerModule)

  test := myInnerModule.io
  
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
