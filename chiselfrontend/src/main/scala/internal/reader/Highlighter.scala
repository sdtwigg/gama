package gama
package internal
package reader

trait Highlighter {
  def BLACK:   String
  def BLUE:    String
  def CYAN:    String
  def GREEN:   String
  def MAGENTA: String
  def RED:     String
  def WHITE:   String
  def YELLOW:  String

  def RESET:   String
}
object Highlighters {
  object Colorful extends Highlighter {
    def BLACK:   String = Console.BLACK
    def BLUE:    String = Console.BLUE
    def CYAN:    String = Console.CYAN
    def GREEN:   String = Console.GREEN
    def MAGENTA: String = Console.MAGENTA
    def RED:     String = Console.RED
    def WHITE:   String = Console.WHITE
    def YELLOW:  String = Console.YELLOW

    def RESET:   String = Console.RESET
  }

  object NonColorful extends Highlighter {
    def BLACK:   String = ""
    def BLUE:    String = ""
    def CYAN:    String = ""
    def GREEN:   String = ""
    def MAGENTA: String = ""
    def RED:     String = ""
    def WHITE:   String = ""
    def YELLOW:  String = ""

    def RESET:   String = ""
  }
}
