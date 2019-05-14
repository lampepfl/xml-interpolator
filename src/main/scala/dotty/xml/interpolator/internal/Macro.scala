package dotty.xml.interpolator.internal

import scala.quoted._
import scala.quoted.autolift._
import scala.quoted.matching._
import scala.tasty.Reflection

import scala.language.implicitConversions

object Macro {
  def impl(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) given Reflection: Expr[scala.xml.Node | scala.xml.NodeBuffer] = {  
    ((strCtxExpr, argsExpr): @unchecked) match {
      case ('{ StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args)) =>
        implicit val args0 = args
        val extracted = Extract(parts)
        val (encoded, offsets) = Encode(extracted)
        val reporter = Reporter(offsets, strCtxExpr)
        val nodes = Parse(encoded)
        val tree = Group(nodes)
        Validate(tree)
        TypeCheck(tree)
        Expand(tree)
    }
  }
}