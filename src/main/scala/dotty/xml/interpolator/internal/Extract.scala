package dotty.xml.interpolator.internal

import scala.quoted._
import scala.quoted.autolift._
import scala.quoted.matching._
import scala.tasty.Reflection

import scala.language.implicitConversions

object Extract {
  def apply(parts: Seq[Expr[String]]) given (reflect: Reflection): Seq[String] = {
    import reflect._
    parts map {
      case Const(part: String) => part
      case expr => throw new NotStaticallyKnownError("Expected statically known String", expr)
    }
  }
}