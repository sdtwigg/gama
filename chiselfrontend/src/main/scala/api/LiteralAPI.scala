package gama
package api
import internal._

trait LiteralAPI {
  val U = LiteralUInt
  val S = LiteralSInt
  val B = LiteralBool
  val LitVec = LiteralVec

  // Pull True and False out
  def True:  Bool = LiteralBool.True
  def False: Bool = LiteralBool.False
}

