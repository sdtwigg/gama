package gama
package backend

object top {
  def main(args: Array[String]) {
    println(lexer.Lexer("Identifier"))
    println(lexer.Lexer("Identifier()"))
    println(lexer.Lexer("Assign(x, 2)"))
    println(lexer.Lexer("Block(Assign(x, 2), Assign(y, x) )"))
    println(lexer.Lexer("Block(Assign(x, 2), Assign(y, x) )").pretty)
    println(lexer.Lexer("A(B(C(D(E(F(G(H))), I, J, K))), L, M(O, P))").pretty)
    println(builder.HWCommandBuilder(lexer.Lexer("EmptyHWCommand")))
    println(builder.HWCommandBuilder(lexer.Lexer("DefLitUInt(a, 1)")))
    println(builder.HWCommandBuilder(lexer.Lexer("DoHWOp(b, ADD-OP, a, a)")))
    println(builder.HWCommandBuilder(lexer.Lexer("""Begin(
      EmptyHWCommand,
      DefLitUInt(a, 1),
      DoHWOp(b, ADD-OP, a, a)
    )""")))
  }
}
