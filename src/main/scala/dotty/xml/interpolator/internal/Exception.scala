package dotty.xml.interpolator.internal

import scala.quoted._

class ParsingError(msg: String, idx: Int) extends Exception(msg)
class ValidationError(msg: String, idx: Int) extends Exception(msg)
class TypeCheckingError(msg: String, expr: Expr[Any]) extends Exception(msg)
class NotStaticallyKnownError(msg: String, expr: Expr[Any]) extends Exception(msg)