package gama

object top {
  def main(args: Array[String]) {

    val myTestModule = ExampleModule()
    println(myTestModule.getActiveJournal)
    println(myTestModule.myInnerModule.getActiveJournal)

/*  // These do not compile, as desired
    val myBMux = Mux(myUInt, mySInt)
    println(getTypeT(myBMux))
    val myBVec = new Vec(Vector(myUInt, myUInt, mySInt)) // doesn't compile as desired
*/
  }
}

class ExampleModule protected () extends Module(UInt()) {
  val uint1 = Wire(UInt(8))
  val uint2 = Wire(UInt())
  val select1 = Wire(Bool())
  val select2 = Wire(Bool())
  val select3 = Wire(Bool())

  val myMux = Mux(select1, uint1, uint2)
  val test = Wire(UInt())
  
  test := select1
  test := uint1 + uint2

  class InnerModule extends Module(UInt()) {
    val uint = Wire(UInt())
  }

  val myInnerModule = Module(new InnerModule)

  test := myInnerModule.io
  
  when(select1) {
    when(select2) {
      test := select1
    }
    test := uint1
  }.elsewhen(Wire(Bool())) {
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

  val myUVec = Vec(myUInt, myUInt, myUInt)
  val myUVec2 = Vec(Vector(myUInt, myUInt, myUInt))
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
