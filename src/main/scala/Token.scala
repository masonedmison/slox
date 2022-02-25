case class Token(
    _type: TokenType,
    lexeme: String,
    literal: Any,
    line: Int
) {
  override def toString: String =
    s"${_type} $lexeme $literal"
}
