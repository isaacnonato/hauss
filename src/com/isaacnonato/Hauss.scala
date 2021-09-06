package com.isaacnonato

object Hauss {
  def main(args: Array[String]) = {

  }

  def run(): Unit = {

  }

  def report(line: Int, where: String, message: String): Unit = {
    println("<line" + line + "> ERROR:" + message)
  }

  def error(token: Token, message: String): Unit = {
    if (token.tokenType == TokenType.EOF) report(token.line, " at end ", message)
  }

  def runtimeError(error: RuntimeError): Unit = {
    System.err.println("<line " + error.token.line + "> " + error.getMessage)
  }

  def error(line: Int, message: String): Unit = {
    report(line, message)
    System.exit(75)
  }
}