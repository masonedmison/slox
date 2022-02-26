import exprs.Expr._

object AstPrinter extends exprs.AST[String] {

  private def paranthenthize(name: String, exps: exprs.Expr*): String =
    exps.foldLeft("(" + name) { case (accS, e) => accS + " " + interpret(e) } + ")"

  // TODO can we make this tail rec?
  def interpret(e: exprs.Expr): String =
    e match {
      case Binary(l, o, r) => paranthenthize(o.lexeme, l, r)
      case Grouping(e)     => paranthenthize("group", e)
      case Literal(v)      =>
        // TODO don't like that v is an object
        paranthenthize(v.toString)
      case NILExp      => "nil"
      case Unary(o, r) => paranthenthize(o.lexeme, r)
    }

}

object demo extends App {

  val exp = Binary(
    Unary(
      Token(TokenType.MINUS, "-", null, 1),
      Literal(123),
    ),
    Token(TokenType.STAR, "*", null, 1),
    Grouping(Literal(45.67)),
  )

  println(AstPrinter.interpret(exp))
}
