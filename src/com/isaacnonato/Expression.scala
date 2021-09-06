package com.isaacnonato
trait Visitor[R] {
  def visitBinaryExpression(expr: Binary): R
  def visitGroupingExpression(expr: Grouping): R
  def visitLiteralExpression(expr: Literal): R
  def visitUnaryExpression(expr: Unary): R
  def visitTernaryExpression(expr: Ternary): R
}
abstract class Expression {

  def accept[R](visitor: Visitor[R]): R
}

abstract class Binary(val left: Expression,
                      val operator: Token, val right: Expression) extends Expression {
  override def accept[R](visitor: Visitor[R]): R = visitor.visitBinaryExpression(this)

}

abstract class Grouping(val expression: Expression) extends Expression {
  override def accept[R](visitor: Visitor[R]): R = visitor.visitGroupingExpression(this)
}

abstract class Literal(val value: Any) extends Expression {
  override def accept[R](visitor: Visitor[R]): R = visitor.visitLiteralExpression(this)
}

abstract class Unary(val operator: Token, val right: Expression) extends Expression {
  override def accept[R](visitor: Visitor[R]): R = visitor.visitUnaryExpression(this)
}

abstract class Ternary(val testExpr: Expression,
                       val trueExpr: Expression, val falseExpr: Expression) extends Expression {
  override def accept[R](visitor: Visitor[R]): R = visitor.visitTernaryExpression(this)
}