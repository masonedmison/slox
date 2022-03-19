import java.util.HashMap

class LoxInstance private (val klass: LoxClass) {
  private val fields: HashMap[String, Any] = new HashMap()

  override def toString: String = klass.name + " instance"

  def get(name: Token): Any =
    if (fields.containsKey(name.lexeme))
      return fields.get(name.lexeme)

    val method = klass.findMethod(name.lexeme)
    if (method != null)
      return method.bind(this)

    throw new RuntimeError(name, s"Undefined property ${name.lexeme}.")

  def set(name: Token, value: Any) = fields.put(name.lexeme, value)
}

object LoxInstance {
  def apply(klass: LoxClass): LoxInstance = new LoxInstance(klass)
}
