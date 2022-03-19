import java.util.HashMap

final case class Environment(enclosing: Environment = null):

  private var values: HashMap[String, Any] = new HashMap()

  def define(name: String, value: Any): Unit = values.put(name, value)

  def get(token: Token): Any =
    if (values.containsKey(token.lexeme))
      values.get(token.lexeme)
    else if (enclosing != null)
      enclosing.get(token)
    else
      throw new RuntimeError(
        token,
        s"Undefined variable  ${token.lexeme}.",
      )

  def getAt(distance: Int, name: String): Any = ancestor(distance).values.get(name)

  def ancestor(dist: Int): Environment = {
    var env = this

    var i = 0

    while (i < dist) {
      env = env.enclosing

      i += 1
    }
    env
  }

  def assign(name: Token, value: Any): Unit =
    if (values.containsKey(name.lexeme))
      values.put(name.lexeme, value)
    else if (enclosing != null)
      enclosing.assign(name, value)
    else
      throw new RuntimeError(
        name,
        s"Undefined variable ${name.lexeme}.",
      )

  def assignAt(
    name: Token,
    value: Any,
    distance: Int,
  ): Unit = ancestor(distance).values.put(name.lexeme, value)
