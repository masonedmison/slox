import java.util.ArrayList

import java.util.HashMap

case class LoxClass(name: String, private val methods: HashMap[String, LoxFunction])
  extends LoxCallable {
  override def toString = name
  def arity: Int = {
    val initializer = findMethod("init")
    if (initializer == null) 0
    else initializer.arity
  }

  def call(interp: Interpreter, args: ArrayList[Any]): Any = {
    val instance = LoxInstance(this)

    val initializer = findMethod("init")
    if (initializer != null)
      initializer.bind(instance).call(interp, args)

    instance
  }

  def findMethod(name: String): LoxFunction =
    if (methods.containsKey(name))
      methods.get(name)
    else
      null

}
