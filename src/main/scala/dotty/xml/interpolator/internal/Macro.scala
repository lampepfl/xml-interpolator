package dotty.xml.interpolator
package internal


import scala.annotation.tailrec
import scala.quoted.*
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.xml.Node.unapplySeq

object Macro {

  def impl(xmlStrCtxExpr: Expr[XML.StringContext], argsExpr: Expr[Seq[Scope ?=> Any]], scope: Expr[Scope])(using qctx: Quotes): Expr[scala.xml.Node | scala.xml.NodeBuffer] = {
    val '{ xml(StringContext(${Varargs(parts)}: _*)) } = xmlStrCtxExpr
    val Varargs(args) = argsExpr
    val (xmlStr, offsets) = encode(parts)

    given XmlContext = new XmlContext(args, scope)
    given Reporter = new Reporter {
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

  def implUnapply(xmlStrCtxExpr: Expr[XML.StringContext], elemExpr: Expr[scala.xml.Node | scala.xml.NodeBuffer], scope: Expr[Scope])(using qctx: Quotes): Expr[Option[Seq[Any]]] = {
    val '{ xml(StringContext(${Varargs(parts)}: _*)) } = xmlStrCtxExpr
    val (xmlStr, offsets) = encode(parts)

    given Reporter = new Reporter {
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

    val parsed = {
      import Parse.{apply => parse}
      import Transform.{apply => transform}
      import Validate.{apply => validate}

      val process = (
        parse
          andThen transform
          andThen validate
      )

      process(xmlStr)
    }

    import scala.quoted.ToExpr.SeqToExpr

    given ToExpr[Tree.Attribute] with
      def apply(attr: Tree.Attribute)(using Quotes): Expr[Tree.Attribute] =
        val Tree.Attribute(name, value) = attr
        val valueExpr = Expr[Seq[Tree.Node]](value)
        '{ Tree.Attribute(${ Expr(name) }, ${ valueExpr }) }

    given ToExpr[Tree.Node] with
      def apply(node: Tree.Node)(using Quotes): Expr[Tree.Node] =
        node match {
          case Tree.Group(nodes) =>
            '{ Tree.Group(${ Expr(nodes) }) }
          case Tree.Elem(name, attrs, children, end) =>
            '{ Tree.Elem(${ Expr(name) }, ${ Expr(attrs) }, ${ Expr(children) }, ${ Expr(end) }) }
          case Tree.Text(text) =>
            '{ Tree.Text(${ Expr(text) }) }
          case Tree.Comment(text) =>
            '{ Tree.Comment(${ Expr(text) }) }
          case Tree.Placeholder(id) =>
            '{ Tree.Placeholder(${ Expr(id) }) }
          case Tree.PCData(data) =>
            '{ Tree.PCData(${ Expr(data) }) }
          case Tree.ProcInstr(target, proctext) =>
            '{ Tree.ProcInstr(${ Expr(target) }, ${ Expr(proctext) }) }
          case Tree.EntityRef(name) =>
            '{ Tree.EntityRef(${ Expr(name) }) }
          case Tree.Unparsed(data) =>
            '{ Tree.Unparsed(${ Expr(data) }) }
        }

    import FillPlaceholders.{apply => fill_placeholders}

    '{
      ${elemExpr} match {
        case e: scala.xml.Node =>
          for {
            map <- fill_placeholders(${ Expr(parsed) }, Seq(e))
          } yield {
            val keys = map.keys.toList.sorted
            assert(!keys.zipWithIndex.exists((k, i) => k != i))
            keys.map(map)
          }
        case e =>
          // TODO: can this be NodeBuffer?
          println("???")
          println(e.getClass)
          ???
      }
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
