package gama.backend
package builder

import gama.backend.lexer.{LexedTree, LexedLeaf, LexedNode}
import gama.IR._

object ModuleBuilder {
  def apply(input: LexedTree) = ???
}

object HWCommandBuilder {
  case class XFormFailure(msg: String) extends Exception(msg)

  def buildAnyStatement(input: LexedTree): HWCommand = input match {
    case LexedLeaf("EmptyHWCommand") => EmptyHWCommand
    case LexedNode("DefLitUInt", name :: value :: Nil) => DefLitUInt(buildString(name), buildBigInt(value))
    case LexedNode("DefLitSInt", name :: value :: Nil) => DefLitSInt(buildString(name), buildBigInt(value))
    case LexedNode("Begin", cmds) => Begin(cmds.map(buildAnyStatement(_)))
    case LexedNode("DoHWOp", name :: op :: args) =>
      DoHWOp(buildString(name), HWOp(buildString(op)), args.map(buildString(_)))
    case LexedNode("Conditionally", pred :: tc :: fc :: Nil) =>
      Conditionally(buildString(pred), buildAnyStatement(tc), buildAnyStatement(fc))

    // Error cases:
    case LexedLeaf(cmd)    => throw XFormFailure(s"Invalid no-arg command ${cmd}")
    case LexedNode(cmd, _) => throw XFormFailure(s"Invalid command ${cmd}")
  }

  def buildString(input: LexedTree): String = input match {
    case LexedLeaf(str)  => str
    case LexedNode(_, _) => throw XFormFailure(s"Expecting String in LeafNode, got ${input}")
  }
  def buildBigInt(input: LexedTree): BigInt = BigInt(buildString(input))
  
  def apply(input: LexedTree): HWCommand = buildAnyStatement(input)
/* // Transformers framework implementation
type Input = LexedTree

  def buildEmptyHWCommand: Transformer[HWCommand] =
    Transformer{case LexedLeaf("EmptyHWCommand") => EmptyHWCommand}
  def apply(input: LexedTree): HWCommand = buildEmptyHWCommand.apply(input) match {
    case Success(result) => result
    case NoSuccess(msg)  => scala.sys.error(msg)
  }
*/
}
