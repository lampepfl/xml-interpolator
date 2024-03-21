package dotty.xml.interpolator
package internal

import scala.quoted._

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object Macro:

  /** ??? */
  def impl(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Scope ?=> Any]], scope: Expr[Scope])
          (using Quotes): Expr[scala.xml.Node | scala.xml.NodeBuffer] =

    (strCtxExpr, argsExpr) match
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>

        val (xmlStr, offsets) = encode(parts)

        given XmlContext = new XmlContext(args, scope)
        given Reporter = new Reporter {
            import quotes.reflect.*

            def error(msg: String, idx: Int): Unit = {
              val (part, offset) = Reporter.from(idx, offsets, parts)
              val pos = part.asTerm.pos
              val (srcF, start) = (pos.sourceFile, pos.start)
              report.error(msg, Position(srcF, start + offset, start + offset + 1))
            }

            def error(msg: String, expr: Expr[Any]): Unit =
              report.error(msg, expr)
           }

        implCore(xmlStr)
  end impl

  def implErrors(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Scope ?=> Any]], scope: Expr[Scope])
                (using Quotes): Expr[List[(Int, String)]] =
    (strCtxExpr, argsExpr) match
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
        val errors = List.newBuilder[Expr[(Int, String)]]
        val (xmlStr, offsets) = encode(parts)
        given XmlContext = new XmlContext(args, scope)
        given Reporter = new Reporter {
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
  end implErrors

  private def implCore(xmlStr: String)(using XmlContext, Reporter, Quotes): Expr[scala.xml.Node | scala.xml.NodeBuffer] =

    import Parse.apply as parse
    import Transform.apply as transform
    import Validate.apply as validate
    import TypeCheck.apply as typecheck
    import Expand.apply as expand

    val interpolate = 
      parse andThen 
      transform andThen 
      validate andThen 
      typecheck andThen
      expand

    interpolate(xmlStr)
  end implCore

  private def encode(parts: Seq[Expr[String]])(using Quotes): (String, Array[Int]) =
    val sb = new StringBuilder()
    val bf = ArrayBuffer.empty[Int]

    def appendPart(part: Expr[String]) =
      bf += sb.length
      sb ++= part.valueOrAbort
      bf += sb.length

    def appendHole(index: Int) = sb ++= Hole.encode(index)

    for (part, index) <- parts.init.zipWithIndex do
      appendPart(part)
      appendHole(index)
    appendPart(parts.last)

    (sb.toString, bf.toArray)
  end encode

end Macro
