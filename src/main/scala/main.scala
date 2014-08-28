package gama

object top {
  def main(args: Array[String]) {
    val test = Wire(UInt(width=16))

    val test2 = UInt(OUTPUT, 8)
    test2.bind(BindSpell.port)

    val test3 = test2.copy.flip
    test3.bind(BindSpell.port)

    val test4 = Wire(test3.copy.flip)

    println(test)
    println(test2)
    println(test3)
    println(test4)

    test4 := test

    val test5 = Wire(UInt(width=16))
    test5 := test3
    
    val test6 = Wire(UInt(width=16))
    val test7 = test5 + test6
  }

}
