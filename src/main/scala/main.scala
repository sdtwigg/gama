package gama

import TypeSafe._

object top {
  def main(args: Array[String]) {
    val test = Wire(UInt(width=16))

    val test2 = UInt(OUTPUT, 8)
    test2.bind(BindSpell.port)

    val test3 = test2.copy.asOutput.flip
    test3.bind(BindSpell.port)

    val test4 = Wire(test3.copy.asOutput.flip.flip)

    println(test)
    println(test2)
    println(test3)
    println(test4)

    test4 := test
    println(test4.getNode.inputs)

    val test5 = Wire(UInt(width=16))
    test5 := test3
    println(test5.getNode.inputs)
    
    val test6 = Wire(UInt(width=16))
    val test7 = test5 + test6
    println(test7.getNode.inputs)

    val test8 = Mux(test3, test7, test6)
    println(test8.getNode.inputs)

    val myb1 = Wire(new MyBundle(32))
    val myb2 = Wire(new MyBundle(8))

    println(myb1.test)
//    val myb3 = Mux(test8, myb1, myb2)

//    println(myb3.in)
/*
    val agga1 = Wire(new AggA)
    val agga2 = Wire(new AggA)
    val aggam = Mux(test3, agga1, agga2)
    
    println(aggam.a.getNode.inputs)
*/
    val myuint1 = Wire(UInt(width=1))
    val myuint2 = Wire(UInt(width=1))
    val mybool1 = Wire(new Bool)
    val mybool2 = Wire(new Bool)

    println(Mux(test3, mybool1, mybool2).getNode.inputs)
    println(Mux(test3, myuint1, myuint2).getNode.inputs)

    println(Mux(test3, myuint1, mybool1).getNode.inputs)
    println(Mux(UInt(), test3, mybool1, myuint1).getNode.inputs)
/*
    val m1 = Mux(test3, myuint1, myuint2)
    val m2 = Mux(test3, myuint1, mybool2)
    val m3 = Mux[UInt](test3, mybool1, mybool2)
    */
  }

}
