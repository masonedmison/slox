object exprs:

  enum Expr:
    case Binary(left: Expr, operator: Token, right: Expr)
    case Grouping(expression: Expr)
    case Literal(value: Any)
    case Unary(operator: Token, right: Expr)
    case NILExp

  trait AST[T]:
    def interpret(e: Expr): T
