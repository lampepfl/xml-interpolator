package dotty.xml.interpolator

import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr

object Interpolator {

  implicit object StringContextOps {
    inline def (ctx: => StringContext) xml (args: => Any*) <: Any =
      ${Interpolator.interpolate('ctx, 'args)}
  }

  private def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[scala.xml.Node | scala.xml.NodeBuffer] = {
    val (strCtx, args) = ExtractStatic(strCtxExpr, argsExpr)
    val (encoded, offsets) = EncodeHole(strCtx)
    val reporter = Reporter(offsets, strCtxExpr)
    val parser = new Parser()
    val parsed = parser.parseAll(parser.XmlExpr, encoded) match {
      case parser.Success(result, _) => result
      case failed : parser.NoSuccess =>
        reporter.error(
          failed.msg,
          failed.next.pos.asInstanceOf[scala.util.parsing.input.OffsetPosition].offset
        )
      throw new QuoteError(failed.msg)
    }
    val grouped = GroupElement(parsed)
    ValidateAttribute(grouped)(reporter)
    TypeCheck(grouped, args)(reflect, reporter)
    Lift(grouped, args)
  }
}