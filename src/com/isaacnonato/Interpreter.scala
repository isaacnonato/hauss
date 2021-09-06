package com.isaacnonato
import com.isaacnonato.Expression

class Interpreter extends Visitor[Any] {
  override def visitBinaryExpression(expr: Binary): Any = {
    val right: Any = expr.right
    val left: Any = expr.left
    val operator: Token = expr.operator

    expr.operator.tokenType match {
      case TokenType.MINUS => {
        checkNumberOperands(left, right, operator)
        double(left) - double(right)
      }
      case TokenType.PLUS => {
        if (left.isInstanceOf[String] == right.isInstanceOf[String]) left.toString + right.toString
        else if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) double(left) + double(right)
        throw new RuntimeError(operator, "Operators must be two numbers or two strings.")
      }
      case TokenType.BANG_EQUAL => {
        checkNumberOperands(left, right, operator)
        !equal(left, right)
      }
      case TokenType.EQUAL_EQUAL => {
        checkNumberOperands(left, right, operator)
        equal(left, right)
      }
      case TokenType.SLASH => {
        checkNumberOperands(left, right, operator)
        double(left) / double(right)
      }
      case TokenType.STAR => {
        checkNumberOperands(left, right, operator)
        double(left) * double(right)
      }
      case TokenType.GREATER => {
        checkNumberOperands(left, right, operator)
        double(left) > double(right)
      }
      case TokenType.GREATER_EQUAL => {
        checkNumberOperands(left, right, operator)
        double(left) >= double(right)
      }
      case TokenType.LESS => {
        checkNumberOperands(left, right, operator)
        double(left) > double(right)
      }
      case TokenType.LESS_EQUAL => {
        checkNumberOperands(left, right, operator)
        double(left) <= double(right)
      }
      case _ => null
    }
  }

  override def visitGroupingExpression(expr: Grouping): Any = eval(expr)

  override def visitLiteralExpression(expr: Literal): Any = expr.value

  override def visitUnaryExpression(expr: Unary): Any = {
    checkNumberOperand(expr.operator, expr.right)
    def right: Any = eval(expr.right)
    expr.operator match {
      case TokenType.MINUS => -double(right)
      case TokenType.BANG => !isTrue(right)
      case _ => null
    }
  }

  override def visitTernaryExpression(expr: Ternary): Any = {
    val falseExpr: Any = eval(expr.falseExpr)
    val trueExpr: Any = eval(expr.trueExpr)
    val testExpr: Any = eval(expr.testExpr)

    if (testExpr.isInstanceOf[Boolean]) {
      if (testExpr.toString.toBoolean) trueExpr
      else falseExpr
    }
    else throw new RuntimeError("The test expression must be a boolean.")
  }

  def interpret(expression: Expression): Unit = {
    try {
      val value: Any = eval(expression)
      println(stringify(value))
    }
    catch {
      case e: RuntimeError => Hauss.runtimeError(e)
    }
  }

  def stringify(obj: Any): String = {
    if (obj == null) return "nil"

    if (obj.isInstanceOf[Double]) {
      var text: String = obj.toString

      if (text.endsWith(".0")) text = text.substring(0, text.length - 2)
      return text
    }
    obj.toString
  }

  def eval(expression: Expression): Any = expression.accept(this)

  def double(a: Any): Double = a.toString.toDouble

  def equal(a: Any, b: Any): Boolean = {
    if (a == null && b == null) return false
    if (a == null) return false
    a.equals(b)
  }

  def isTrue(obj: Any): Boolean = {
    if (obj == null) return false
    if (obj.isInstanceOf[Boolean]) return obj.toString.toBoolean
    true
  }

  def checkNumberOperand(operator: Token, operand: Any): Unit = {
    if (operand.isInstanceOf[Double]) return
    throw new RuntimeError(operator, "Operator must be a number.")
  }

  def checkNumberOperands(left: Any, right: Any, operator: Token): Unit = {
    if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) return
    throw new RuntimeError(operator, "Operators must be numbers.")
  }
}
