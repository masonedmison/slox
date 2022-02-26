import java.nio.file.Files

import java.nio.file.Path
import java.nio.charset.Charset
import java.nio.file.Paths
import java.io.InputStreamReader
import java.io.BufferedReader
import java.util.ArrayList

object Lox:

  var hadError = false

  def main(args: Array[String]): Unit =
    if (args.length > 1)
      println("Usage: slox [script]")
    else if (args.length == 1)
      runFile(args(0))
    else
      runPrompt

  private def runFile(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset()))

    if (hadError)
      System.exit(65)
  }

  private def runPrompt: Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    var running = true

    while (running) {
      print("> ")
      val line = reader.readLine

      if (line == null)
        running = false
      else
        run(line)

      hadError = false
    }
  }

  private def run(source: String): Unit = {
    val scanner = new Scanner(source)
    val tokens: ArrayList[Token] = scanner.scanTokens

    tokens.toArray.foreach(println)
  }

  def error(line: Int, message: String) = report(line, "", message)

  def error(token: Token, message: String): Unit =
    if (token._type == TokenType.EOF)
      report(token.line, " at and", message)
    else {
      report(token.line, s" at '${token.lexeme}'", message)
    }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println(s"[line $line] Error $where: $message")
    hadError = true
  }
