import exprs.Expr

import exprs.AST

object Interpreter extends AST[Any] {

  def evaluate(expr: Expr): Unit =
    try {
      val value = interpret(expr)
      println(stringify(value))
    } catch { case err: RuntimeError => Lox.runtimeError(err) }

  def interpret(expr: Expr): Any =
    expr match {
      case Expr.Literal(v)  => v
      case Expr.Grouping(e) => interpret(e)
      case Expr.Unary(op, r) =>
        val right = interpret(r)
        (op._type, right) match {
          case (TokenType.MINUS, r: Double) => -r
          case (TokenType.MINUS, r) => throw new RuntimeError(op, "Operand must be a number.")
          case (TokenType.BANG, r)  => !isTruthy(r)
          case _ => throw new RuntimeError(op, "Unexpected operand and expression.") // unreachable
        }
      case Expr.Binary(l, op, r) =>
        val lVal = interpret(l)
        val rVal = interpret(r)

        (lVal, rVal) match {
          case (l: Double, r: Double) =>
            op._type match {
              case TokenType.MINUS         => l - r
              case TokenType.STAR          => l * r
              case TokenType.SLASH         => l / r
              case TokenType.PLUS          => l + r
              case TokenType.GREATER       => l > r
              case TokenType.GREATER_EQUAL => l >= r
              case TokenType.LESS          => l < r
              case TokenType.LESS_EQUAL    => l <= r
              case TokenType.EQUAL_EQUAL   => l == r
              case TokenType.BANG_EQUAL    => l != r
              case _ =>
                throw new RuntimeError(
                  op,
                  s"Unsupported operator ${op._type} for numbers.",
                ) // unreachable
            }
          case (l: String, r: String) =>
            op._type match {
              case TokenType.PLUS        => l + r
              case TokenType.BANG_EQUAL  => !isEqual(l, r)
              case TokenType.EQUAL_EQUAL => isEqual(l, r)
              case _ =>
                throw new RuntimeError(op, "Unexpected binary operator on String") // unreachable
            }
          case (l, r) =>
            op._type match {
              case TokenType.BANG_EQUAL  => !isEqual(l, r)
              case TokenType.EQUAL_EQUAL => isEqual(l, r)
              case _ => throw new RuntimeError(op, s"Unexpected operator between $l and $r.")
            }
        }
      case Expr.NILExp => null
    }

  private def isEqual(a: Any, b: Any): Boolean =
    if (a == null && b == null)
      true
    else if (a == null)
      false
    else
      a.equals(b)

  private def isTruthy(o: Any): Boolean =
    if (o == null)
      false
    else if (o.isInstanceOf[Boolean])
      o.isInstanceOf[Boolean]
    else
      true

  private def stringify(o: Any): String =
    o match {
      case null => "nil"
      case d: Double =>
        var text = d.toString
        if (text.endsWith(".0")) {
          text = text.substring(0, text.length - 2)
          text
        } else text
      case _ => o.toString

    }

}
