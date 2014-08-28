package gama

trait Opcode
trait UnaryOpcode extends Opcode
trait BinaryOpcode extends Opcode

object Opcodes {
  object UPlus extends BinaryOpcode
}
