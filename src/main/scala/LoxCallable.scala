import java.util.ArrayList

trait LoxCallable:
  def arity: Int
  def call(interp: Interpreter, args: ArrayList[Any]): Any
