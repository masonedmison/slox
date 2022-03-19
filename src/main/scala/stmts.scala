import exprs.Expr

import java.util.ArrayList

object stmts:

  enum Stmt:
    case Expression(e: Expr)
    case Print(e: Expr)
    case Var(name: Token, init: Expr)
    case Block(statements: ArrayList[Stmt])
    case If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt)
    case While(condition: Expr, body: Stmt)
    case Function(name: Token, params: ArrayList[Token], body: ArrayList[Stmt])
    case ClassDecl(name: Token, methods: ArrayList[Stmt.Function])
    case Return(keyword: Token, value: Expr)

  trait StmtInterpreter[O]:
    def interpret(e: Stmt): O
