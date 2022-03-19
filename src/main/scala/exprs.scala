import java.util.ArrayList

object exprs:

  enum Expr:
    case Binary(left: Expr, operator: Token, right: Expr)
    case Grouping(expression: Expr)
    case Literal(value: Any)
    case Unary(operator: Token, right: Expr)
    case NILExp
    case Variable(name: Token)
    case Assign(name: Token, expression: Expr)
    case Logical(left: Expr, operator: Token, right: Expr)
    case Call(callee: Expr, paren: Token, arguments: ArrayList[Expr])
    case Get(expr: Expr, name: Token)
    case Set(obj: Expr, name: Token, value: Expr)
    case This(keyword: Token)

  trait ExprInterpreter[O]:
    def interpret(expr: Expr): O
