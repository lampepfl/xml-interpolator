package dotty.xml.interpolator

import scala.quoted._
import scala.tasty.Reflection

object ExtractStatic {

  def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): (StringContext, List[Expr[Any]]) = {
    (getStaticStringContext(strCtxExpr), getArgsList(argsExpr))
  }

  private def getStaticStringContext(strCtxExpr: Expr[StringContext])(implicit reflect: Reflection): StringContext = {
    import reflect._
    strCtxExpr.unseal.underlyingArgument match {
      case Apply(_, List(Typed(Repeated(strCtxArgTrees, Inferred()), _))) =>
        val strCtxArgs = strCtxArgTrees.map {
          case Literal(Constant.String(str)) => str
          case tree => throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal)
        }
        StringContext(strCtxArgs: _*)
      case tree =>
        throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal)
    }
  }

  private def getArgsList(argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): List[Expr[Any]] = {
    import reflect._
    argsExpr.unseal.underlyingArgument match {
      case Typed(Repeated(args, _), _) => args.map(_.seal)
      case tree => throw new NotStaticlyKnownError("Expected statically known argument list", tree.seal)
    }
  }

  private class NotStaticlyKnownError(msg: String, expr: Expr[Any]) extends Exception(msg)
}