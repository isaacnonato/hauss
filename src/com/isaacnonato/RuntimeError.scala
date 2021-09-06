package com.isaacnonato

class RuntimeError(val token: Token, val message: String) extends RuntimeException {
  def this(message: String) = this(null, super(message))
}
