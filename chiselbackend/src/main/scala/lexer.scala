package gama.backend.lexer
import scala.util.parsing.combinator._

sealed trait LexedTree {def pretty: String}
sealed case class LexedLeaf(name: String) extends LexedTree {def pretty = name}
sealed case class LexedNode(name: String, children: scala.collection.immutable.List[LexedTree]) extends LexedTree {
  // Use List so can unapply with ::
  def pretty = { name + "(\n" +
      children.map(i => i.pretty.split("\n").map("  " + _ + "\n").reduce(_+_)).reduce(_+_) +
    ")"
  }
}

object Lexer extends RegexParsers {
  def token : Parser[String] = "[a-zA-Z0-9_.$-]+".r

  def ltSingle : Parser[LexedTree] = token <~ (not("(") | "()") ^^ {i => LexedLeaf(i)}
  def ltArgs   : Parser[LexedTree] = token ~ "(" ~ rep1sep(ltTree, ",") ~ ")" ^^ {
    case id ~ "(" ~ args ~ ")" => LexedNode(id, args.toList)
  }
  def ltTree : Parser[LexedTree] = ltSingle | ltArgs

  def apply(input: String): LexedTree = parseAll(ltTree, input) match {
    case Success(result, _)  => result
    case failure : NoSuccess => (scala.sys.error(failure.msg))
  }
}
/*
sealed trait ASTexpression
case class ASTliteral(lit: Int) extends ASTexpression
case class ASTident(name: String) extends ASTexpression

sealed trait ASTstatement
case class ASTassign(ident: ASTident, rv: ASTexpression) extends ASTstatement
case class ASTblock(statements: Seq[ASTstatement]) extends ASTstatement

object ASTParser extends JavaTokenParsers {
  def astIdent   : Parser[ASTident]   = ident       ^^ { i => ASTident(i) }
  def astLiteral : Parser[ASTliteral] = wholeNumber ^^ { i => ASTliteral(i.toInt) }
  def astExpression : Parser[ASTexpression] = astLiteral | astIdent

  def astAssign       : Parser[ASTassign] = "Assign(" ~> astIdent ~ "," ~ astExpression <~ ")" ^^ {
    case ident ~ "," ~ expression => ASTassign(ident, expression)
  }
  def astBlock        : Parser[ASTblock]  = "Block(" ~> rep1sep(astStatement, ",")  <~ ")" ^^ {
    case statements => ASTblock(statements)
  }
  def astStatement : Parser[ASTstatement] = astAssign | astBlock

  def apply(input: String): ASTstatement = parseAll(astStatement, input) match {
    case Success(result, _)  => result
    case failure : NoSuccess => (scala.sys.error(failure.msg))
  }
}
*/
// TODO: Add more elements, better expression tracking, and better error handling
// TODO: Consider lexer and then parser stages
