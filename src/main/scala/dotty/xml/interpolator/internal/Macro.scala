package dotty.xml.interpolator
package internal

import scala.quoted._

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object Macro {

  def impl(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Scope ?=> Any]], scope: Expr[Scope])(using qctx: Quotes): Expr[scala.xml.Node | scala.xml.NodeBuffer] = {
    ((strCtxExpr, argsExpr): @unchecked) match {
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
        val (xmlStr, offsets) = encode(parts)
        implicit val ctx: XmlContext = new XmlContext(args, scope)
        implicit val reporter: Reporter = new Reporter {
          import quotes.reflect._

          def error(msg: String, idx: Int): Unit = {
            val (part, offset) = Reporter.from(idx, offsets, parts)
            val pos = part.asTerm.pos
            val (srcF, start) = (pos.sourceFile, pos.start)
            report.error(msg, Position(srcF, start + offset, start + offset + 1))
          }

          def error(msg: String, expr: Expr[Any]): Unit = {
            report.error(msg, expr)
          }
        }
        implCore(xmlStr)
    }
  }

  def implErrors(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Scope ?=> Any]], scope: Expr[Scope])(using qctx: Quotes): Expr[List[(Int, String)]] = {
    ((strCtxExpr, argsExpr): @unchecked) match {
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
        val errors = List.newBuilder[Expr[(Int, String)]]
        val (xmlStr, offsets) = encode(parts)
        implicit val ctx: XmlContext = new XmlContext(args, scope)
        implicit val reporter: Reporter = new Reporter {
          import quotes.reflect._

          def error(msg: String, idx: Int): Unit = {
            val (part, offset) = Reporter.from(idx, offsets, parts)
            val start = part.asTerm.pos.start - parts(0).asTerm.pos.start
            errors += Expr((start + offset, msg))
          }

          def error(msg: String, expr: Expr[Any]): Unit = {
            val pos = expr.asTerm.pos
            errors += Expr((pos.start, msg))
          }
        }
        implCore(xmlStr)
        Expr.ofList(errors.result())
    }
  }

  private def implCore(xmlStr: String)(using XmlContext, Reporter, Quotes): Expr[scala.xml.Node | scala.xml.NodeBuffer] = {

    import Parse.{apply => parse}
    import Transform.{apply => transform}
    import Validate.{apply => validate}
    import TypeCheck.{apply => typecheck}
    import Expand.{apply => expand}

    val interpolate = (
      parse
        andThen transform
        andThen validate
        andThen typecheck
        andThen expand
    )

    interpolate(xmlStr)
  }

  private def encode(parts: Seq[Expr[String]])(using Quotes): (String, Array[Int]) = {
    val sb = new StringBuilder()
    val bf = ArrayBuffer.empty[Int]

    def appendPart(part: Expr[String]) = {
      bf += sb.length
      sb ++= part.valueOrAbort
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
