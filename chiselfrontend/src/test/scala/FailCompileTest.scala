import org.scalatest._
import org.scalatest.Assertions
import org.junit.Test

import gama._
import gama.api._
import gama.annotations._

class FailCompileTest extends TestSuite {
  test("MustEncloseWire") {
    assertTypeError("val a = Wire(UInt())")
  }
  test("MustEncloseReg", MarkedTag) {
    assertTypeError("val a = Reg(UInt())")
  }
}
