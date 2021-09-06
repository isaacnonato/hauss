package com.isaacnonato

object TokenType extends Enumeration {
  type TokenType = Value;
  // Single-character tokens
  val LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  AT_SYMBOL, DOT, MINUS, PLUS, SEMICOLON, COLON,
  STAR, SLASH, QUESTION_MARK, DOUBLE_QUOTE, COMMA = Value;

  // One of two characters
  val EQUAL, EQUAL_EQUAL, BANG_EQUAL,
  BANG, GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL = Value;

  // Literals
  val IDENTIFIER, STRING, NUMBER = Value;

  // Keywords
  val AND, CLASS, IF, ELSE, BOOLEAN_TRUE, BOOLEAN_FALSE, DEF, RECDEF, ENDF,
  LOG, RETURN, THIS, WHILE, ENDWHILE, FOR, ENDFOR, LET, NIL = Value;

  val EOF = Value;

}