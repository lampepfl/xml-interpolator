package dotty.xml.interpolator.internal

import scala.quoted._
import scala.quoted.autolift._
import scala.quoted.matching._
import scala.tasty.Reflection

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object Macro {

  def impl(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) given (reflect: Reflection): Expr[scala.xml.Node | scala.xml.NodeBuffer] = {
    ((strCtxExpr, argsExpr): @unchecked) match {
      case ('{ StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args)) =>
        val (xmlStr, offsets) = encode(parts)
        implicit val arguments: Seq[Expr[Any]] = args
        implicit val reporter: Reporter = new Reporter {
          import reflect._

          def error(msg: String, idx: Int): Unit = {
            val (part, offset) = Reporter.from(idx, offsets, parts)
            val pos = part.unseal.pos
            val (srcF, start) = (pos.sourceFile, pos.start)
            reflect.error(msg, srcF, start + offset, start + offset + 1)
          }

          def error(msg: String, expr: Expr[Any]): Unit = {
            reflect.error(msg, expr.unseal.pos)
          }
        }
        implCore(xmlStr)
    }
  }

  def implErrors(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) given (reflect: Reflection): Expr[List[(Int, Int, String)]] = {
    ((strCtxExpr, argsExpr): @unchecked) match {
      case ('{ StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args)) =>
        val errors = List.newBuilder[Expr[(Int, Int, String)]]
        val (xmlStr, offsets) = encode(parts)
        implicit val arguments: Seq[Expr[Any]] = args
        implicit val reporter: Reporter = new Reporter {
          import reflect._

          def error(msg: String, idx: Int): Unit = {
            val (part, offset) = Reporter.from(idx, offsets, parts)
            val pos = part.unseal.pos
            val (srcF, start) = (pos.sourceFile, pos.start)
            errors += '{ Tuple3(${start + offset}, ${start + offset + 1}, $msg) }
          }

          def error(msg: String, expr: Expr[Any]): Unit = {
            val pos = expr.unseal.pos
            errors += '{ Tuple3(${pos.start}, ${pos.end}, $msg) }
          }
        }
        implCore(xmlStr)
        errors.result().toExprOfList
    }
  }

  private def implCore(xmlStr: String) given Seq[Expr[Any]] given Reporter given Reflection: Expr[scala.xml.Node | scala.xml.NodeBuffer] = {
    var tree = Parse(xmlStr); tree = Transform(tree)
    Validate(tree)
    TypeCheck(tree)
    Expand(tree)

    /** TODO
      val interpolate = (
        Parse
          andThen Transform
          andThen Validate
          andThen TypeCheck
          andThen Expand
      )
      interpolate(xmlStr)
    */
  }

  private def encode(parts: Seq[Expr[String]]) given Reflection: (String, Array[Int]) = {
    val sb = new StringBuilder()
    val bf = ArrayBuffer.empty[Int]

    def appendPart(part: Expr[String]) = {
      val Const(value: String) = part
      bf += sb.length
      sb ++= value
      bf += sb.length
    }

    def appendHole(index: Int) = {
      sb ++= Hole.encode(index)
    }

    for ((part, index) <- parts.init.zipWithIndex) {
      appendPart(part)
      appendHole(index)
    }
    appendPart(parts.last)

    (sb.toString, bf.toArray)
  }
}