package gama

object top {
  def main(args: Array[String]) {
    import scala.reflect.runtime.universe._
    def getTypeT[A: TypeTag](in: A) = typeTag[A]

    val myWire = new UInt(new Wire(RawUBits(None)))
    val myReg = Reg.makeReg(myWire)
    println(getTypeT(myWire).tpe)
    println(getTypeT(myReg).tpe)
    
    val mySWire = new SInt(new Wire(RawSBits(None)))
    val mySReg = Reg.makeReg(mySWire)
    println(getTypeT(mySWire).tpe)
    println(getTypeT(mySReg).tpe)

    val myMux = Mux.makeMux(myReg,myWire)
    println(getTypeT(myMux).tpe)

  }

}

