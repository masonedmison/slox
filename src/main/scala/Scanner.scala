import java.util.ArrayList

import TokenType.*

final case class Scanner(source: String) {
  final var tokens: ArrayList[Token] = new ArrayList()
  private var start = 0
  private var current = 0
  private var line = 1

  private val keywords = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE,
  )

  def scanTokens: ArrayList[Token] = {
    while (!isAtEnd) {
      start = current
      scanToken
    }
    tokens.add(Token(EOF, "", null, line))
    tokens
  }

  def scanToken: Unit =
    advance match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' =>
        addToken(
          if (_match('='))
            BANG_EQUAL
          else
            BANG
        )
      case '=' =>
        addToken(
          if (_match('='))
            EQUAL_EQUAL
          else
            EQUAL
        )
      case '<' =>
        addToken(
          if (_match('='))
            LESS_EQUAL
          else
            LESS
        )
      case '>' =>
        addToken(
          if (_match('='))
            GREATER_EQUAL
          else
            GREATER
        )
      case '/' =>
        if (_match('/')) {
          while (peek != '\n' && !isAtEnd)
            advance
        } else
          addToken(SLASH)
      case ' '  => ()
      case '\r' => ()
      case '\t' => ()
      case '\n' =>
        line += 1
        ()
      case '"' => string
      case c =>
        if (isDigit(c))
          number
        else if (isAlpha(c))
          identifier
        else
          Lox.error(line, "Unexpected characeter.")
    }

  private def identifier: Unit = {
    while (isAlphaNumeric(peek))
      advance

    val text = source.substring(start, current)
    keywords.get(text).getOrElse(IDENITIFIER) match {
      case TRUE  => addToken(TRUE, true)
      case FALSE => addToken(FALSE, false)
      case _type => addToken(_type)
    }

  }

  private def number: Unit = {
    while (isDigit(peek))
      advance

    if (peek == '.' && isDigit(peekNext)) {
      advance
      while (isDigit(peek))
        advance
    } else {
      val value = source.substring(start, current).toDouble
      addToken(NUMBER, value)
    }

  }

  private def string: Unit = {
    while (peek != '"' && !isAtEnd)
      if (peek == '\n')
        line += 1
      else
        advance

    if (isAtEnd) {
      Lox.error(line, "Unterminated string.")
    } else {
      advance
      val value = source.substring(start + 1, current - 1)
      addToken(STRING, value)
    }
  }

  private def _match(expected: Char): Boolean =
    if (isAtEnd) false
    else if (source.charAt(current) != expected) false
    else {
      current += 1
      true
    }

  private def peek: Char =
    if (isAtEnd)
      '\u0000'
    else
      source.charAt(current)

  private def peekNext: Char =
    if (current + 1 >= source.length)
      '\u0000'
    else
      source.charAt(current + 1)

  private def isAlpha(c: Char): Boolean =
    (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'z') ||
      (c == '_')

  private def isAlphaNumeric(c: Char) = isAlpha(c) || isDigit(c)

  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  private def isAtEnd: Boolean = current >= source.length

  private def advance: Char = {
    // current += 1
    // source.charAt(current)
    val c = source.charAt(current)
    current += 1
    c
  }

  private def addToken(_type: TokenType): Unit = addToken(_type, null)

  private def addToken(_type: TokenType, literal: Any): Unit = {
    val text = source.slice(start, current)

    tokens.add(Token(_type, text, literal, line))
  }

}
