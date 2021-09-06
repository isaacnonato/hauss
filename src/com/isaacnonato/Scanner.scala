package com.isaacnonato

import com.isaacnonato.Scanner.keywords

import java.util

object Scanner {
  val keywords: util.HashMap[String, TokenType.Value] = new util.HashMap[String, TokenType.Value]()
  keywords.put("and", TokenType.AND)
  keywords.put("class", TokenType.CLASS)
  keywords.put("if", TokenType.IF)
  keywords.put("else", TokenType.ELSE)
  keywords.put("True", TokenType.BOOLEAN_TRUE)
  keywords.put("False", TokenType.BOOLEAN_FALSE)
  keywords.put("def", TokenType.DEF)
  keywords.put("recdef", TokenType.RECDEF)
  keywords.put("endf", TokenType.ENDF)
  keywords.put("log", TokenType.LOG)
  keywords.put("return", TokenType.RETURN)
  keywords.put("this", TokenType.THIS)
  keywords.put("let", TokenType.LET)
  keywords.put("while", TokenType.WHILE)
  keywords.put("endwhile", TokenType.ENDWHILE)
  keywords.put("for", TokenType.FOR)
  keywords.put("endfor", TokenType.ENDFOR)
}

abstract class Scanner(source: String) {
  var tokens: util.ArrayList[Token]
  var start: Int = 0
  var line: Int = 1
  var current: Int = 0

  def scanTokens: util.ArrayList[Token] = {
    while (!isAtEnd)  {
      start = current
      scanToken()
    }
    tokens.add(new Token("", tokenType = TokenType.EOF, line, null))
    tokens
  }

  keywords.put("and", TokenType.AND)
  keywords.put("class", TokenType.CLASS)
  keywords.put("if", TokenType.IF)
  keywords.put("else", TokenType.ELSE)
  keywords.put("True", TokenType.BOOLEAN_TRUE)
  keywords.put("False", TokenType.BOOLEAN_FALSE)
  keywords.put("def", TokenType.DEF)
  keywords.put("recdef", TokenType.RECDEF)
  keywords.put("endf", TokenType.ENDF)
  keywords.put("log", TokenType.LOG)
  keywords.put("return", TokenType.RETURN)
  keywords.put("this", TokenType.THIS)
  keywords.put("let", TokenType.LET)
  keywords.put("while", TokenType.WHILE)
  keywords.put("endwhile", TokenType.ENDWHILE)
  keywords.put("for", TokenType.FOR)
  keywords.put("endfor", TokenType.ENDFOR)

  def scanToken(): Unit = {
    val c: Char = advance

    c match {
      case '(' => addToken(TokenType.LEFT_PAREN)
      case ')' => addToken(TokenType.RIGHT_PAREN)
      case '{' => addToken(TokenType.LEFT_BRACE)
      case '}' => addToken(TokenType.RIGHT_BRACE)
      case '@' => addToken(TokenType.AT_SYMBOL)
      case '.' => addToken(TokenType.DOT)
      case ';' => addToken(TokenType.SEMICOLON)
      case ':' => addToken(TokenType.COLON)
      case '*' => addToken(TokenType.STAR)
      case '-' => addToken(TokenType.MINUS)
      case '+' => addToken(TokenType.PLUS)
      case ',' => addToken(TokenType.COMMA)
      case '/' => addToken(TokenType.SLASH)
      case '?' => addToken(TokenType.QUESTION_MARK)
      case '"' => string
      case '!' => addToken(if (isMatch('=')) {
        TokenType.BANG_EQUAL
      } else {
        TokenType.BANG
      })
      case '>' => addToken(if (isMatch('=')) {
        TokenType.GREATER_EQUAL
      }  else {
        TokenType.GREATER
      })
      case '<' => addToken(if (isMatch('=')) {
        TokenType.LESS_EQUAL
      } else {
        TokenType.LESS
      })
      case '=' => addToken(if (isMatch('=')) {
        TokenType.EQUAL_EQUAL
      } else {
        TokenType.EQUAL
      })
      case _ => {
        if (isNumber(c)) number()
        else if (isAlpha(c)) identifier()
        else Hauss.error(line, "Unexpected token.")
      }
    }
  }

  def isMatch(expected: Char): Boolean = {
    if (isAtEnd) false
    if (source.charAt(current) != expected) false

    current = current + 1
    true
  }

  def lookahead: Char = {
    if (isAtEnd) return '\u0000'
    source.charAt(current)
  }

  def lookaheadNext: Char = {
    if (current + 1 >= source.length) return '\u0000'
    source.charAt(current + 1)
  }

  def string: Unit = {
    // TODO: implement multi line strings

    while (lookahead != '"' && !isAtEnd) {
      if (lookahead == '\n') {
        Hauss.error(line, "Unterminated string.")
      }
      advance
    }
    val text: String = source.substring(start + 1, current - 1)
    addToken(TokenType.STRING, text)

  }

  def isNumber(c: Char): Boolean = c >= '0' && c <= '9'

  def number(): Unit = {
    while (isNumber(lookahead)) {
      advance
    }
    if (lookahead == '.') {
      addToken(TokenType.NUMBER, source.substring(start, current-1).toDouble)
    } else if (isNumber(lookaheadNext)) {
      if (isNumber(lookahead)) advance
    }
  }

  def identifier(): Unit = {
    while (isAlphaNumeric(lookahead)) advance
    val text: String = source.substring(start, current)
    val tokenType: TokenType.Value = keywords.get(text)

    if (tokenType == null) addToken(TokenType.IDENTIFIER, text)
    else addToken(tokenType)
  }

  def isAlpha(c: Char): Boolean = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c == '_'

  def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isNumber(c)

  def addToken(tokenType: TokenType.Value): Unit = addToken(tokenType, null)

  def addToken(tokenType: TokenType.Value, literal: Any, text: String = source.substring(start, current)): Unit =
    tokens.add(new Token(text, tokenType, line, literal))

  def advance: Char = source.charAt(current + 1)

  def isAtEnd: Boolean = current >= source.length
}