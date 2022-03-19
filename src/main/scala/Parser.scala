import exprs.Expr

// import exprs.LiteralValue
import util.control.Breaks._

import stmts.Stmt
import scala.collection.JavaConverters._

import TokenType.*
import java.util.ArrayList
import scala.annotation.varargs
import java.util.Arrays

class Parser(tokens: ArrayList[Token]) {
  private var current: Int = 0

  // TODO don't like this
  def parse: ArrayList[Stmt] = {
    val statements: ArrayList[Stmt] = new ArrayList()

    while (!isAtEnd)
      statements.add(declaration)

    statements
  }

  private case object ParseError extends RuntimeException

  private def declaration: Stmt =
    try if (_match(VAR))
      varDeclaration
    else if (_match(FUN))
      function("function")
    else if (_match(CLASS))
      clsStatement
    else
      statement
    catch {
      case ParseError =>
        syncronize
        null // yikes...

    }

  private def statement: Stmt =
    if (_match(PRINT))
      printStatement
    else if (_match(LEFT_BRACE))
      Stmt.Block(block)
    else if (_match(IF))
      ifStatement
    else if (_match(WHILE))
      whileStatement
    else if (_match(FOR))
      forStatement
    else if (_match(RETURN))
      returnStatement
    else
      expressionStatement

  private def ifStatement: Stmt = {
    consume(LEFT_PAREN, "Expt '(' after if statement")
    val exp = expression
    consume(RIGHT_PAREN, "Expect ')' after conditional expression.")

    val thenBranch = statement

    var elseBranch: Stmt = null
    if (_match(ELSE))
      elseBranch = statement

    Stmt.If(exp, thenBranch, elseBranch)
  }

  private def printStatement: Stmt = {
    val value = expression
    consume(SEMICOLON, s"Expect ; after value.")

    Stmt.Print(value)
  }

  private def returnStatement: Stmt = {
    val keyword = previous

    var expr: Expr = null

    if (!check(SEMICOLON))
      expr = expression

    consume(SEMICOLON, "Expect ';' after return value.")

    Stmt.Return(keyword, expr)
  }

  private def whileStatement: Stmt = {
    consume(LEFT_PAREN, "Expect '(' before while condition")
    val expr = expression
    consume(RIGHT_PAREN, "Expect ')' after while condition")

    val body = statement

    Stmt.While(expr, body)
  }

  private def forStatement: Stmt = {
    consume(LEFT_PAREN, "Expect '(' before for-loop expressions")

    val initializer: Stmt =
      if (_match(SEMICOLON))
        null
      else if (_match(VAR))
        varDeclaration
      else
        expressionStatement

    var condition =
      if (!check(SEMICOLON))
        expression
      else
        null

    consume(SEMICOLON, "Expect semicolon after loop condition.")

    val increment =
      if (!check(RIGHT_PAREN))
        expression
      else
        null

    consume(RIGHT_PAREN, "Expect ')' after for-loop expressions.")

    var body = statement

    if (increment != null)
      body = Stmt.Block(new ArrayList(List(body, Stmt.Expression(increment)).asJava))

    if (condition == null)
      condition = Expr.Literal(true)

    body = Stmt.While(condition, body)

    if (initializer != null)
      body = Stmt.Block(new ArrayList(List(initializer, body).asJava))

    body
  }

  private def clsStatement: Stmt = {
    val name = consume(IDENITIFIER, "Expect class name.")

    consume(LEFT_BRACE, "Expect '{' before class body.")

    val methods: ArrayList[Stmt.Function] = new ArrayList()
    while (!check(RIGHT_BRACE) && !isAtEnd)
      methods.add(function("method"))

    consume(RIGHT_BRACE, "Expect '}' after class body.")

    Stmt.ClassDecl(name, methods)

  }

  private def varDeclaration: Stmt = {
    val name = consume(IDENITIFIER, "Expect variable name.")

    var init: Expr = null // TODO don't like this...

    if (_match(EQUAL)) {
      init = expression
    }

    consume(SEMICOLON, "Expect ';' after variable declaration")

    Stmt.Var(name, init)
  }

  private def expressionStatement: Stmt = {
    val value = expression
    consume(SEMICOLON, s"Expect ; after value.")

    Stmt.Expression(value)
  }

  private def expression: Expr = assignment

  private def function(kind: String): Stmt.Function = {
    val name = consume(IDENITIFIER, s"Expect $kind name.")

    consume(LEFT_PAREN, s"Expect '(' after $kind name.")
    val params: ArrayList[Token] = new ArrayList()
    if (!check(RIGHT_PAREN)) {

      params.add(consume(IDENITIFIER, "Expect parameter name."))
      while (_match(COMMA)) {
        if (params.size >= 255)
          error(peek, "Can't have more than 255 paramters.")
        params.add(consume(IDENITIFIER, "Expect parameter name."))
      }
    }
    consume(RIGHT_PAREN, "Expect ')' after parameters.")

    consume(LEFT_BRACE, s"Expect '{' before $kind name.")
    val body = block

    Stmt.Function(name, params, body)
  }

  private def block: ArrayList[Stmt] = {
    val statements: ArrayList[Stmt] = new ArrayList()

    while (!check(RIGHT_BRACE) && !isAtEnd)
      statements.add(declaration)

    consume(RIGHT_BRACE, "Expect '}' after block.")
    statements
  }

  private def assignment: Expr = {
    val expr = or

    if (_match(EQUAL)) {
      val equals = previous

      val value = assignment

      expr match {
        case Expr.Variable(name) => Expr.Assign(name, value)
        case Expr.Get(obj, name) => Expr.Set(obj, name, value)
        case _ =>
          throw error(equals, "Invalid assignment target.") // TODO this shouldn't actually throw
      }
    } else expr
  }

  private def or: Expr = {
    var expr = and

    while (_match(OR)) {
      val op = previous
      val right = and

      expr = Expr.Logical(expr, op, right)
    }

    expr
  }

  private def and: Expr = {
    var expr = equality

    while (_match(AND)) {
      val op = previous
      val right = equality

      expr = Expr.Logical(expr, op, right)
    }

    expr
  }

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
  private def previous: Token = tokens.get(current - 1)

  private def error(token: Token, msg: String) = {
    Lox.error(token, msg)

    ParseError
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

  private def peek = tokens.get(current)

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
    } else call

  private def finishCall(callee: Expr): Expr = {
    val args: ArrayList[Expr] = new ArrayList()

    if (!check(RIGHT_PAREN)) {
      // do...
      args.add(expression)
      while (_match(COMMA))
        if (args.size >= 255)
          error(peek, "Can't have more than 255 arugments")
        args.add(expression)
    }

    val paren = consume(RIGHT_PAREN, "Expect ')' after arguments.")

    Expr.Call(callee, paren, args)
  }

  private def call: Expr = {
    var expr = primary

    breakable {
      while (true)
        if (_match(LEFT_PAREN)) expr = finishCall(expr)
        else if (_match(DOT)) {
          val name = consume(IDENITIFIER, "Expect property after '.'")
          expr = Expr.Get(expr, name)
        } else break
    }

    expr
  }

  private def primary: Expr =
    if (_match(FALSE)) Expr.Literal(false)
    else if (_match(TRUE)) Expr.Literal(true)
    else if (_match(NIL)) Expr.NILExp
    else if (_match(NUMBER, STRING)) Expr.Literal(previous.literal)
    else if (_match(IDENITIFIER)) Expr.Variable(previous)
    else if (_match(THIS)) Expr.This(previous)
    else if (_match(LEFT_PAREN)) {
      val expr = expression
      consume(RIGHT_PAREN, "Expect ')' after expression")
      Expr.Grouping(expr)
    } else throw error(peek, "Expect expression.")

}

object Parser {
  def apply(tokens: ArrayList[Token]) = new Parser(tokens)
}
