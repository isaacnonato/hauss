package com.isaacnonato

class Token(val lexeme: String, val tokenType: TokenType.Value, val line: Int, val literal: Any) {
  override def toString: String = tokenType + " " + lexeme + " " + literal
}