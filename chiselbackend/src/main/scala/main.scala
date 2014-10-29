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
  }
}
