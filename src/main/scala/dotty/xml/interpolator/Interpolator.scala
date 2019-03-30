package dotty.xml.interpolator

import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr
import scala.quoted.Toolbox.Default._

object Interpolator extends MacroStringInterpolator[scala.xml.Node | scala.xml.NodeBuffer] {

  class StringContextOps(strCtx: => StringContext) {
    inline def xml(args: Any*) <: Any = ${Interpolator('strCtx, 'args)}
  }
  implicit inline def SCOps(strCtx: => StringContext): StringContextOps = new StringContextOps(strCtx)

  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[scala.xml.Node | scala.xml.NodeBuffer] = {
    val encoded = EncodeHole(strCtx)
    val parser  = new Parser()
    val parsed  = parser.parseAll(parser.XmlExpr, encoded) match {
      case parser.Success(result, _) => result
      case failed : parser.NoSuccess => throw new QuoteError(failed.msg)
    }
    val grouped = GroupElement(parsed)
    ValidateAttribute(grouped)
    TypeCheck(grouped, args)
    Lift(grouped, args)
  }
}