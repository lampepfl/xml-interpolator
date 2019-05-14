package dotty.xml.interpolator.internal

import scala.quoted._
import scala.quoted.matching._
import scala.tasty.Reflection

trait Reporter {
  def error(msg: String, offset: Int): Unit
  def error(msg: String, expr: Expr[Any]): Unit
}

object Reporter {
  def apply(offsets: Array[Int], strCtxExpr: Expr[StringContext])(implicit reflect: Reflection): Reporter = {
    import reflect._
    val parts = getStringContextPartsExpr(strCtxExpr)
    new Reporter{
      def error(msg: String, offset: Int): Unit = {
        val index = offsets.lastIndexWhere(offset >= _)
        val isWithinHoleOrAtTheEnd = index % 2 != 0
        val (source, start, end) = 
          if (isWithinHoleOrAtTheEnd) {
            val partIndex = (index - 1) / 2
            val position = parts(partIndex).pos
            val source = position.sourceFile
            val start = position.start + (offset - offsets(index - 1))
            val end = start + 1
            (source, start, end)
          } else {
            val partIndex = index / 2
            val position = parts(partIndex).pos
            val source = position.sourceFile
            val start = position.start + (offset - offsets(index))
            val end = start + 1
            (source, start, end)
          }
        reflect.error(msg, source, start, end)
      }
      def error(msg: String, expr: Expr[Any]): Unit = {
        reflect.error(msg, expr.unseal.pos)
      }
    }
  }
  private def getStringContextPartsExpr(strCtxExpr: Expr[StringContext])(implicit reflect: Reflection) = {
    import reflect._
    (strCtxExpr.unseal.underlyingArgument : @unchecked) match {
      case Apply(_, List(Typed(Repeated(parts, Inferred()), _))) => parts
    }
  }
}