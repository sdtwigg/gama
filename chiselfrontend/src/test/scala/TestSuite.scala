import org.scalatest._
import org.scalatest.FunSuite
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Tag

import gama._

abstract class TestSuite extends FunSuite with AssertionsForJUnit

object MarkedTag extends Tag("MARKED")
// run only these tests with test -- -n MARKED
