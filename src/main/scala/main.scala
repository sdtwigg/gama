package gama

object top {
  def main(args: Array[String]) {
    val test = UInt(width=16)
    test.bind((e:Element[NodeStore]) => (new Wire(e.generateStorage)))

    val test2 = UInt(OUTPUT)
    test2.bind((e:Element[NodeStore]) =>
      (new Port(e.generateStorage, e.getDirection.getOrElse(throw new Exception("Cannot create Port from non-directioned Element")))
      )
    )
  }
}
