package gama

object top {
  def main(args: Array[String]) {
    import scala.reflect.runtime.universe._

    val myWire = new Bits(new Wire(RawBits(None)))

    def getTypeT[A: TypeTag](in: A) = typeTag[A]
    println(getTypeT(myWire).tpe)

    val myReg = Reg.makeReg(myWire)
    println(getTypeT(myReg).tpe)
  }

}
