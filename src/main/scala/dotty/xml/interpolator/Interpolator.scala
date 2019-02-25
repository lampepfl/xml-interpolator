package dotty.xml.interpolator

import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr
import scala.quoted.Toolbox.Default._

object Interpolator extends MacroStringInterpolator[String] {

  class StringContextOps(strCtx: => StringContext) {
    inline def xml(args: Any*): String = ~{Interpolator('(strCtx), '(args))}
  }
  implicit inline def SCOps(strCtx: => StringContext): StringContextOps = new StringContextOps(strCtx)

  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[String] =
    '{(~{strCtx.toExpr}).s(~{args.toExprOfList}: _*)}
}