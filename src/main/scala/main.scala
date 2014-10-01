package gama

object top {
  def main(args: Array[String]) {
    import scala.reflect.runtime.universe._
    def getTypeT[A: TypeTag](in: A) = typeTag[A]

    implicit val em = EnclosingModule(None)

    val myUInt = UInt()
    val myUWire = Wire(myUInt)
    val myUReg = Reg(myUInt)
    val myUMux = Mux(myUWire, myUReg)

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
    val myUVecMux = Mux(myUVec, myUVec)
    val myUVecReg = Reg(myUVec)
    println(getTypeT(myUVecMux))
    println(getTypeT(myUVecReg))

    val myBool = Bool()

/*  // These do not compile, as desired
    val myBMux = Mux(myUInt, mySInt)
    println(getTypeT(myBMux))
    val myBVec = new Vec(Vector(myUInt, myUInt, mySInt)) // doesn't compile as desired
*/
  }
}

