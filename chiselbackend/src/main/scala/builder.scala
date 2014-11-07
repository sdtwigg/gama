package gama.backend
package builder

import gama.backend.lexer.{LexedTree, LexedLeaf, LexedNode}
import gama.IR._

object ModuleBuilder {
  def apply(input: LexedTree) = ???
}

object HWCommandBuilder {
  case class XFormFailure(loc: String, msg: String) extends Exception(s"$msg @ $loc")
  object XFormFailure {def apply(msg: String): XFormFailure = XFormFailure("", msg)}

  def adjustXFE[T](input: LexedTree, work: =>T): T = try{work} catch {
    case XFormFailure(loc, msg) => throw XFormFailure(s"${input.name}.$loc", msg)
  }

  def buildString(input: LexedTree): String = input match {
    case LexedLeaf(str)  => str
    case LexedTree(loc, _) => throw XFormFailure(s"Expecting String in LeafNode, got ${input}")
  }
  def buildBigInt(input: LexedTree): BigInt = BigInt(buildString(input))
  def buildInt(input: LexedTree): Int       = buildString(input).toInt

  def buildIODirection(input: LexedTree): IODirection = adjustXFE(input,{input match {
    case LexedLeaf("InputIO")    => InputIO
    case LexedLeaf("OutputIO")   => OutputIO
    case LexedLeaf("UnknownDir") => UnknownDir
    case LexedTree(loc, _) => throw XFormFailure(s"Expecting IODirection, got ${input}")
  }})

  def buildIOWidth(input: LexedTree): IOWidth = adjustXFE(input,{input match {
    case LexedLeaf("UnknownWidth") => UnknownWidth
    case LexedNode("IntWidth", value :: Nil) => IntWidth(buildInt(value))
    case LexedTree(loc, _) => throw XFormFailure(s"Expecting IOWidth, got ${input}")
  }})

  def buildHWType(input: LexedTree): HWType = adjustXFE(input,{input match {
    case LexedLeaf("UnknownHWType") => UnknownHWType
    case LexedNode("UIntType", w :: Nil) => UIntHWType(buildIOWidth(w))
    case LexedNode("SIntType", w :: Nil) => SIntHWType(buildIOWidth(w))
    case LexedNode("BundleType", ports) => BundleType(ports.map(buildHWPort(_)))
    case LexedTree(loc, _) => throw XFormFailure(s"Expecting HWType, got ${input}")
  }})

  def buildHWPort(input: LexedTree): HWPort = adjustXFE(input,{input match {
    case LexedNode("HWPort", name :: dir :: hwtype :: Nil) =>
      HWPort(buildString(name), buildIODirection(dir), buildHWType(hwtype))
    case LexedTree(loc, _) =>
      throw XFormFailure(s"Expecting HWPort(name: String, IODirection, HWType), got ${input}")
  }})

  def buildAnyStatement(input: LexedTree): HWCommand = adjustXFE(input,{input match {
    case LexedNode("DefLitUInt", name :: value :: Nil) =>
      DefLitUInt(buildString(name), buildBigInt(value))
    case LexedNode("DefLitSInt", name :: value :: Nil) =>
      DefLitSInt(buildString(name), buildBigInt(value))
    case LexedNode("DefWire", name :: hwtype :: Nil) =>
      DefWire(buildString(name), buildHWType(hwtype))
    case LexedNode("DefRegister", name :: hwtype :: Nil) =>
      DefRegister(buildString(name), buildHWType(hwtype))
    case LexedNode("DefInstance", name :: mname :: Nil) =>
      DefInstance(buildString(name), buildString(mname))
    case LexedNode("DefMemory", name :: hwtype :: size :: Nil) =>
      DefMemory(buildString(name), buildHWType(hwtype), buildInt(size))
    case LexedNode("DefVector", name :: hwtype :: args) =>
      DefVector(buildString(name), buildHWType(hwtype), args.map(buildString(_)))
    case LexedNode("DefAccessor", name :: src :: dir :: idx :: Nil) =>
      DefAccessor(buildString(name), buildString(src), buildIODirection(dir), buildString(idx))

    case LexedNode("ConnectMany", idx :: loc :: exps) =>
      ConnectMany(buildString(idx), buildString(loc), exps.map(buildString(_)))
    case LexedNode("ManyConnect", idx :: args) =>
      ManyConnect(buildString(idx), args.init.map(buildString(_)), buildString(args.last))

    case LexedNode("Begin", cmds)
      => Begin(cmds.map(buildAnyStatement(_)))
    case LexedNode("DoHWOp", name :: op :: args) =>
      DoHWOp(buildString(name), HWOp(buildString(op)), args.map(buildString(_)))
    case LexedNode("Conditionally", pred :: tc :: fc :: Nil) =>
      Conditionally(buildString(pred), buildAnyStatement(tc), buildAnyStatement(fc))
    case LexedLeaf("EmptyHWCommand") => EmptyHWCommand

    // Error cases: Handle no arg and arg cases separated
    case LexedLeaf(cmd)    => throw XFormFailure(s"Invalid no-arg command ${input}")
    case LexedNode(cmd, _) => throw XFormFailure(s"Invalid command ${input}")
  }})
  
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
