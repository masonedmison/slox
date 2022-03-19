import java.util.HashMap

import exprs.ExprInterpreter
import exprs.Expr
import stmts.StmtInterpreter
import stmts.Stmt
import java.util.ArrayList
import java.util.Stack
import util.control.Breaks._
import Resolver._

class Resolver private (interpreter: Interpreter)
  extends ExprInterpreter[Unit]
  with StmtInterpreter[Unit] {

  private val scopes: Stack[HashMap[String, Boolean]] = new Stack
  private var currentFunction = FunctionType.NONE

  def interpret(stmt: Stmt): Unit =
    stmt match {
      case Stmt.Block(stmts)             => interpretBlock(stmts)
      case varStmt @ Stmt.Var(_, _)      => interpretVarStmt(varStmt)
      case func @ Stmt.Function(_, _, _) => interpretFunction(func)
      case Stmt.Expression(expr)         => interpret(expr)
      case Stmt.If(cond, thenStmt, elseStmt) =>
        interpret(cond)
        interpret(thenStmt)
        if (elseStmt != null)
          interpret(elseStmt)
      case Stmt.Print(expr)        => interpret(expr)
      case ret @ Stmt.Return(_, _) => interpretReturn(ret)
      case Stmt.While(cond, stmt) =>
        interpret(cond)
        interpret(stmt)
      case cls @ Stmt.ClassDecl(_, _) => interpretCls(cls)
    }

  def interpret(expr: Expr): Unit =
    expr match {
      case varExp @ Expr.Variable(_)  => interpretVarExpr(varExp)
      case assign @ Expr.Assign(_, _) => interpretAssign(assign)
      case Expr.Binary(l, _, r) =>
        interpret(l)
        interpret(r)
      case Expr.Call(callee, _, params) =>
        interpret(callee)
        params.forEach(interpret)
      case Expr.Grouping(e) => interpret(expr)
      case Expr.Literal(_)  => ()
      case Expr.Logical(l, _, r) =>
        interpret(l)
        interpret(r)
      case Expr.Unary(_, e) => interpret(e)
      case Expr.NILExp      => ()
      case Expr.Get(e, _)   => interpret(e)
      case Expr.Set(obj, name, value) =>
        interpret(obj)
        interpret(value)
      case _this @ Expr.This(_) => resolveLocal(_this, _this.keyword)
    }

  private def interpretReturn(stmt: Stmt.Return): Unit = {
    if (currentFunction == FunctionType.NONE)
      Lox.error(stmt.keyword, "Can't return from top-level code.")

    if (stmt.value != null)
      interpret(stmt.value)
  }

  private def interpretBlock(stmts: ArrayList[Stmt]): Unit = {
    beginScope
    resolve(stmts)
    endScope
  }

  private def interpretVarStmt(stmt: Stmt.Var): Unit = {
    declare(stmt.name)
    if (stmt.init != null)
      interpret(stmt.init)

    define(stmt.name)
  }

  def interpretFunction(stmt: Stmt.Function): Unit = {
    declare(stmt.name)
    define(stmt.name)
    resolveFunction(stmt, FunctionType.FUNCTION)
  }

  def interpretCls(stmt: Stmt.ClassDecl): Unit = {
    declare(stmt.name)
    define(stmt.name)

    beginScope
    scopes.peek.put("this", true)
    stmt.methods.forEach(meth => resolveFunction(meth, FunctionType.METHOD))
    endScope
  }

  private def interpretVarExpr(varExp: Expr.Variable): Unit = {
    if (!scopes.isEmpty && scopes.peek.get(varExp.name) != false) {
      Lox.error(varExp.name, "Can't read local variable in it's own initializer.")
    }
    resolveLocal(varExp, varExp.name)
  }

  private def interpretAssign(expr: Expr.Assign): Unit = {
    interpret(expr.expression)
    resolveLocal(expr, expr.name)
  }

  def resolve(stmts: ArrayList[Stmt]): Unit = stmts.forEach(interpret)

  def resolveLocal(expr: Expr, name: Token): Unit = {
    var i = scopes.size() - 1

    breakable {
      while (i >= 0) {
        if (scopes.get(i).containsKey(name.lexeme)) {
          interpreter.resolve(expr, scopes.size() - 1 - i)
          break
        }
        i -= 1
      }
    }
  }

  def resolveFunction(function: Stmt.Function, funcType: Resolver.FunctionType): Unit = {
    val enclosingFunction = currentFunction
    currentFunction = funcType
    beginScope
    function.params.forEach { p =>
      declare(p); define(p)
    }
    resolve(function.body)

    endScope
    currentFunction = enclosingFunction
  }

  private def beginScope: Unit = scopes.push(new HashMap[String, Boolean])

  private def endScope: Unit = scopes.pop

  private def declare(name: Token): Unit = {
    if (scopes.isEmpty)
      return

    val scope = scopes.peek

    if (scope.containsKey(name.lexeme)) {
      Lox.error(name, "Already a variable with this name in the scope.")
    }

    scope.put(name.lexeme, false)
  }

  private def define(name: Token): Unit = {
    if (scopes.isEmpty)
      return

    scopes.peek().put(name.lexeme, true)
  }

}

object Resolver {

  private[Resolver] enum FunctionType:
    case NONE
    case FUNCTION
    case METHOD

  def apply(interpreter: Interpreter): Resolver = new Resolver(interpreter)
}
