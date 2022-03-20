import exprs.Expr

import stmts.Stmt
import java.util.ArrayList
import java.util.HashMap

class Interpreter() extends exprs.ExprInterpreter[Any] with stmts.StmtInterpreter[Unit] {
  val globals = new Environment()
  private val locals: HashMap[Expr, Integer] = new HashMap()
  private var environment = globals

  private def evaluate(expr: Expr): Unit =
    try {
      val value = interpret(expr)
      // println(stringify(value))
    } catch { case err: RuntimeError => Lox.runtimeError(err) }

  def evaluate(stmts: ArrayList[Stmt]): Unit =
    try stmts.forEach(interpret)
    catch { case err: RuntimeError => Lox.runtimeError(err) }

  def resolve(expr: Expr, depth: Int): Unit = locals.put(expr, depth)

  def interpret(stmt: Stmt): Unit =
    stmt match {
      case Stmt.Print(e)      => println(stringify(interpret(e)))
      case Stmt.Expression(e) => evaluate(e)
      case Stmt.Var(tok, init) =>
        var value: Any = null
        if (init != null)
          value = interpret(init)
        environment.define(tok.lexeme, value)
      case Stmt.Block(stmts) => executeBlock(stmts, new Environment(environment))
      case Stmt.If(cond, thenBranch, elseBranch) =>
        if (isTruthy(interpret(cond)))
          interpret(thenBranch)
        else if (elseBranch != null)
          interpret(elseBranch)
        else
          ()
      case Stmt.While(cond, body) =>
        while (isTruthy(interpret(cond)))
          interpret(body)
      case func @ Stmt.Function(name, _, _) =>
        val loxFunc = LoxFunction.apply(func, environment, isInitializer = false)
        environment.define(name.lexeme, loxFunc)
      case cls @ Stmt.ClassDecl(_, _) => interpretCls(cls)
      case Stmt.Return(keyword, expr) =>
        var value: Any = null
        if (expr != null) {
          value = interpret(expr)
        }

        throw new Return(value)
    }

  // TODO Break this up into smaller functions...
  def interpret(expr: Expr): Any =
    expr match {
      case Expr.Literal(v)                     => v
      case Expr.Grouping(e)                    => interpret(e)
      case unary @ Expr.Unary(_, _)            => interpretUnary(unary)
      case call @ Expr.Call(callee, tok, args) => interpretFunctionCall(call, tok, args)
      case binOp @ Expr.Binary(_, _, _)        => interpretBinOp(binOp)
      case get @ Expr.Get(_, _)                => interpretGet(get)
      case set @ Expr.Set(_, _, _)             => interpretSet(set)
      case varExpr @ Expr.Variable(varName)    => lookupVariable(varName, varExpr)
      case assign @ Expr.Assign(tok, expr)     => interpretAssign(assign)
      case logical @ Expr.Logical(_, _, _)     => interpretLogical
      case _this @ Expr.This(_)                => interpretThis(_this)
      case Expr.NILExp                         => null
    }

  private def lookupVariable(name: Token, expr: Expr): Any = {
    val dist = locals.get(expr)
    if (dist != null) {
      environment.getAt(dist, name.lexeme)
    } else
      globals.get(name)
  }

  private def interpretFunctionCall(expr: Expr.Call, tok: Token, argExps: ArrayList[Expr]) = {
    val callee = interpret(expr.callee)

    val args: ArrayList[Any] = new ArrayList()

    argExps.forEach(x => args.add(interpret(x)))

    if (!callee.isInstanceOf[LoxCallable])
      throw new RuntimeError(expr.paren, "Can only call functions and classes.")

    val function = callee.asInstanceOf[LoxCallable]

    if (args.size != function.arity)
      throw new RuntimeError(
        expr.paren,
        s"Expected ${function.arity} arguments but got ${args.size}.",
      )

    function.call(this, args)
  }

  private def interpretLogical(expr: Expr.Logical): Any = {
    val left = interpret(expr.left)

    if (expr.operator == TokenType.OR && isTruthy(left))
      left
    else if (expr.operator == TokenType.AND && !isTruthy(left))
      left
    else
      interpret(expr.right)
  }

  private def interpretUnary(expr: Expr.Unary): Any = {
    val right = interpret(expr.right)
    (expr.operator._type, right) match {
      case (TokenType.MINUS, r: Double) => -r
      case (TokenType.MINUS, r) =>
        throw new RuntimeError(expr.operator, "Operand must be a number.")
      case (TokenType.BANG, r) => !isTruthy(r)
      case _ =>
        throw new RuntimeError(expr.operator, "Unexpected operand and expression.") // unreachable
    }
  }

  private def interpretAssign(assign: Expr.Assign): Any = {
    val distance = locals.get(assign)
    val value = interpret(assign.expression)
    if (distance != null)
      environment.assignAt(assign.name, value, distance)
    else
      globals.assign(assign.name, value)
  }

  private def interpretBinOp(expr: Expr.Binary): Any = {
    val lVal = interpret(expr.left)
    val rVal = interpret(expr.right)
    val op = expr.operator

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
  }

  private def interpretThis(expr: Expr.This): Any = lookupVariable(expr.keyword, expr)

  private def interpretCls(cls: Stmt.ClassDecl): Unit = {
    environment.define(cls.name.lexeme, null)

    val methods: HashMap[String, LoxFunction] = new HashMap()

    cls.methods.forEach { meth =>
      methods.put(
        meth.name.lexeme,
        LoxFunction.apply(meth, environment, isInitializer = meth.name.lexeme == "init"),
      )
    }

    val klass = LoxClass(cls.name.lexeme, methods)

    environment.assign(cls.name, klass)

  }

  private def interpretGet(expr: Expr.Get): Any = {
    val obj = interpret(expr.expr)

    if (obj.isInstanceOf[LoxInstance])
      obj.asInstanceOf[LoxInstance].get(expr.name)
    else
      throw new RuntimeError(expr.name, "Only instances can have properties.")
  }

  private def interpretSet(expr: Expr.Set): Any = {
    val obj = interpret(expr.obj)

    if (obj.isInstanceOf[LoxInstance])
      val value = interpret(expr.value)
      obj.asInstanceOf[LoxInstance].set(expr.name, value)
      value
    else throw new RuntimeError(expr.name, "Only instances can have fields.")

  }

  def executeBlock(stmts: ArrayList[Stmt], env: Environment): Unit = {
    val prev = this.environment
    try {
      this.environment = env
      stmts.forEach(interpret)
    } finally this.environment = prev

  }

  private def isEqual(a: Any, b: Any): Boolean =
    if (a == null && b == null)
      true
    else if (a == null)
      false
    else
      a.equals(b)

  // something sus here...
  private def isTruthy(o: Any): Boolean =
    if (o.isInstanceOf[Boolean])
      o.asInstanceOf[Boolean]
    else if (o == null)
      false
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

object Interpreter {

  def apply(): Interpreter = {
    val interp = new Interpreter()

    interp
      .globals
      .define(
        "clock",
        new LoxCallable {
          def arity: Int = 0

          def call(interpter: Interpreter, args: ArrayList[Any]): Any =
            System.currentTimeMillis / 1000.0

          override def toString = "<native fn>"
        },
      )

    interp
  }

}
