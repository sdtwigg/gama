package gama

object top {
  def main(args: Array[String]) {
    val test = new UInt(new Wire(RawBits(Option(16))))
    val test2 = test.asInput
  }
}
