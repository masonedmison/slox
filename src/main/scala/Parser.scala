import exprs.Expr

import TokenType.*

class Parser(tokens: IndexedSeq[Token]) {
  private var current: Int = 0

  // TODO don't like this
  def parse: Expr =
    try expression
    catch {
      case err: ParseError => null
    }

  private final case class ParseError() extends RuntimeException

  private def expression: Expr = equality

  private def _match(tTypes: TokenType*): Boolean =
    tTypes.exists(check) match {
      case true  => advance; true
      case false => false
    }

  private def check(tType: TokenType) =
    if (isAtEnd)
      false
    else
      peek._type == tType

  private def isAtEnd: Boolean = peek._type == EOF
  private def previous: Token = tokens(current - 1)

  private def error(token: Token, msg: String): ParseError = {
    Lox.error(token, msg)

    ParseError()
  }

  // TODO this sucks...
  private def syncronize: Unit = {
    advance

    while (!isAtEnd) {
      if (previous._type == SEMICOLON)
        return

      peek._type match {
        case CLASS | FOR | FUN | IF | PRINT | RETURN | VAR | WHILE => return
        case _                                                     => advance
      }
    }
  }

  private def advance: Token =
    if (!isAtEnd)
      current += 1
    previous

  private def peek = tokens(current)

  private def consume(tType: TokenType, msg: String) =
    if (check(tType))
      advance
    else
      throw error(peek, msg)

  private def equality: Expr = {
    var expr = comparison
    while (_match(BANG_EQUAL, EQUAL_EQUAL)) {
      val op = previous
      val right = comparison
      expr = Expr.Binary(expr, op, right)
    }

    expr

  }

  private def comparison: Expr = {
    var expr = term

    while (_match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val op = previous
      val right = term
      expr = Expr.Binary(expr, op, right)
    }
    expr
  }

  private def term: Expr = {
    var expr = factor

    while (_match(MINUS, PLUS)) {
      val op = previous
      val right = factor
      expr = Expr.Binary(expr, op, right)
    }

    expr
  }

  private def factor: Expr = {
    var expr = unary

    while (_match(SLASH, STAR)) {
      val op = previous
      val right = unary
      expr = Expr.Binary(expr, op, right)
    }

    expr
  }

  private def unary: Expr =
    if (_match(BANG, MINUS)) {
      val op = previous
      val expr = unary

      Expr.Unary(op, expr)
    } else primary

  private def primary: Expr =
    if (_match(FALSE)) Expr.Literal(false)
    else if (_match(TRUE)) Expr.Literal(true)
    else if (_match(NIL)) Expr.NILExp
    else if (_match(NUMBER, STRING)) Expr.Literal(previous.literal)
    else if (_match(LEFT_PAREN)) {
      val expr = expression
      consume(RIGHT_PAREN, "Expect ')' after expression")
      Expr.Grouping(expr)
    } else throw error(peek, "Expect expression.")

}

object Parser {
  def apply(tokens: IndexedSeq[Token]) = new Parser(tokens)
}
