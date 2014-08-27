package gama

object top {
  def main(args: Array[String]) {
    val test = UInt(width=16)
    test.bind(BindSpell.wire)

    val test2 = UInt(OUTPUT, 8)
    test2.bind(BindSpell.port)

    val test3 = test2.copy.flip
    test3.bind(BindSpell.port)

    val test4 = test3.copy.flip
    test4.bind(BindSpell.wire)

    println(test)
    println(test2)
    println(test3)
    println(test4)
  }

}
