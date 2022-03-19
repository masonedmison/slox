import exprs.Expr._

object AstPrinter extends exprs.ExprInterpreter[String] {

  private def paranthenthize(name: String, exps: exprs.Expr*): String =
    exps.foldLeft("(" + name) { case (accS, e) => accS + " " + interpret(e) } + ")"

  // TODO can we make this tail rec?
  def interpret(e: exprs.Expr): String =
    e match {
      case Binary(l, o, r) => paranthenthize(o.lexeme, l, r)
      case Grouping(e)     => paranthenthize("group", e)
      // case Literal(v)      =>
      //   // TODO don't like that v is an object
      //   paranthenthize(v.toString)
      case NILExp            => "nil"
      case Unary(o, r)       => paranthenthize(o.lexeme, r)
      case Variable(tok)     => tok.lexeme // TODO not sure this is what we want
      case Assign(tok, expr) => tok.lexeme + " = " + interpret(expr)
    }

}
