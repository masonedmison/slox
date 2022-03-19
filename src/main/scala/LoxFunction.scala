import stmts.Stmt

import java.util.ArrayList

class LoxFunction private (declaration: Stmt.Function, closure: Environment) extends LoxCallable {

  def arity: Int = declaration.params.size

  def call(interp: Interpreter, args: ArrayList[Any]): Any = {
    val env = new Environment(closure)

    var i = 0

    while (i < declaration.params.size) {
      env.define(declaration.params.get(i).lexeme, args.get(i))
      i += 1
    }

    try interp.executeBlock(declaration.body, env)
    catch {
      case Return(value) => return value
    }

    return null

  }

  def bind(instance: LoxInstance): LoxFunction = {
    val env = new Environment(closure)
    env.define("this", instance)

    LoxFunction.apply(declaration, env)
  }

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
}

object LoxFunction {
  def apply(decl: Stmt.Function, closure: Environment): LoxFunction = new LoxFunction(decl, closure)
}
