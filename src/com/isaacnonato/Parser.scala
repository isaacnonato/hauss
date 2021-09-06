package com.isaacnonato
/*
* Hauss expression grammar:
*
* expression    -> equality ;
* equality      -> comparison ( ( "!=" | "==" ) comparison)* ;
* comparison    -> term ( ( ">" | ">=" | "<" | "<=" ) term)* ;
* ternary       -> comparison ( "?" term ":" term)* ;
* term          -> factor ( ( "+" | "-" ) factor)* 2;
* factor        -> unary ( ( "/" | "*" ) unary )* ;
* unary         -> ( "!" | "-" ) unary ;
* primary       -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
*
* */
class ParseError extends RuntimeException {}

class Parser(tokens: List[Token]) {

  var current: Int = 0;

  def parse: Expression = {
    try {
      expression
    } catch {
      case e: Exception => {
        println(e.printStackTrace())
        throw e
      }
    }
  }

  private def expression: Expression = equality

  private def lookahead: Token = tokens(current)

  private def comparison: Expression = {
    var expr: Expression = term

    while (`match`(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)) {
      val operator: Token = previous
      val right: Expression = term
      expr = new Binary(expr, operator, right) {}
    }

    while (`match`(TokenType.QUESTION_MARK)) {
      ternary
    }
    expr
  }

  private def term: Expression = {
    var expr: Expression = factor

    while (`match`(TokenType.MINUS, TokenType.PLUS)) {
      val operator: Token = previous
      val right: Expression = factor
      expr = new Binary(expr, operator, right) {}
    }
    expr
  }

  private def factor: Expression = {
    var expr: Expression = unary

    while (`match`(TokenType.SLASH, TokenType.STAR)) {
      val operator: Token = previous
      val right = unary
      expr = new Binary(expr, operator, right) {}
    }
    expr
  }

  private def unary: Expression = {
    if (`match`(TokenType.MINUS, TokenType.BANG)) {
      val operator: Token = previous
      val right: Expression = unary
      return new Unary(operator, right) {}
    }
    primary
  }

  private def primary: Expression = {
    if (`match`(TokenType.BOOLEAN_FALSE)) return new Literal(false) {}
    if (`match`(TokenType.BOOLEAN_TRUE)) return new Literal(true) {}
    if (`match`(TokenType.NIL)) return new Literal(null) {}

    if (`match`(TokenType.STRING, TokenType.NUMBER)) {
      return new Literal(previous.literal) {}
    }

    if (`match`(TokenType.LEFT_PAREN)) {
      val expr: Expression = expression
      consume(TokenType.RIGHT_PAREN, "Missing ')'.")
      return new Grouping(expr) {}
    }

    throw error (lookahead, "Expression expected.")
  }

  private def ternary: Expression = {
    var expr: Expression = comparison
    val trueExpr: Expression = term
      if (`match`(TokenType.COLON)) {
        val falseExpr: Expression = term
        expr = new Ternary(expr, trueExpr, falseExpr) {}
      }
    expr
  }


  private def equality: Expression = {
    var expr: Expression = comparison

    while (`match`(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
      val operator: Token = previous
      val right: Expression = comparison
      expr = new Binary(expr, operator, right) {}
    }
    expr
  }

  // Utils

  def error(token: Token, message: String): ParseError = {
    Hauss.error(token, message)
    new ParseError
  }

  private def isAtEnd: Boolean = lookahead.tokenType == TokenType.EOF

  private def consume(tokenType: TokenType.Value, message: String): Token = {
    if (check(tokenType)) advance
    throw error(lookahead, message) // TODO
  }

  private def check(tokenType: TokenType.Value): Boolean = {
    if (isAtEnd) return false
    lookahead.tokenType == tokenType
  }

  private def previous: Token = tokens(current - 1)

  private def advance: Token = {
    if (!isAtEnd) current = current + 1
    previous
  }

  private def `match`(types: TokenType.Value*): Boolean = {
    for (tokenType <- types) {
      if (check(tokenType)) {
        advance
        return true
      }
    }
    false
  }

}