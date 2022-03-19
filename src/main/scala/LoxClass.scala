import java.util.ArrayList

import java.util.HashMap

case class LoxClass(name: String, private val methods: HashMap[String, LoxFunction])
  extends LoxCallable {
  override def toString = name
  def arity: Int = 0
  def call(interp: Interpreter, args: ArrayList[Any]): Any = LoxInstance(this)

  def findMethod(name: String): LoxFunction =
    if (methods.containsKey(name))
      methods.get(name)
    else
      null

}
