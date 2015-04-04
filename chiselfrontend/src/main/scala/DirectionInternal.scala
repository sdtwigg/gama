package gama
package internal

sealed trait DirectionIO
object DirectionIO {
  case object Input  extends DirectionIO
  case object Output extends DirectionIO
}
