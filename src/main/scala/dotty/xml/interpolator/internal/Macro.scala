package dotty.xml.interpolator
package internal


import scala.annotation.tailrec
import scala.quoted.*
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.xml.Node.unapplySeq

object Macro {

  def impl(xmlStrCtxExpr: Expr[XML.StringContext], argsExpr: Expr[Seq[Scope ?=> Any]], scope: Expr[Scope])(using qctx: Quotes): Expr[scala.xml.Node | scala.xml.NodeBuffer] = {
    println("=============================================================")

    val '{ xml(StringContext(${Varargs(parts)}: _*)) } = xmlStrCtxExpr
    val Varargs(args) = argsExpr
    val (xmlStr, offsets) = encode(parts)

    println(s"parts: $parts")
    println(s"args: $args")
    println(s"xmlStr: $xmlStr")
    println(s"offsets: ${java.util.Arrays.toString(offsets)}")

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

//  def compareNodes(skeleton: Tree.Node, data: scala.xml.Node): Boolean =
//    (skeleton, data) match
//      case ()
//
//  @tailrec
//  def compareAttrs(skeleton: Seq[Tree.Attribute], data: scala.xml.MetaData): Boolean =
//    (skeleton, data) match
//      case (seq@Seq(_, _*), scala.xml.PrefixedAttribute(pre, key, value, next)) =>
//        val idx = seq.indexWhere { attr =>
//          attr.pre == pre && attr.key == key && compareNodes(attr.value, value)
//        }
//        if idx == -1 then
//          false
//        else
//          val newSeq = seq.take(idx) ++ seq.drop(idx + 1)
//          compareAttrs(newSeq, next)
//      case (seq@Seq(_, _*), scala.xml.UnprefixedAttribute(key, value, next)) =>
//        val idx = seq.indexWhere { attr =>
//          attr.key == key && compareNodes(attr.value, value)
//        }
//        if idx == -1 then
//          false
//        else
//          val newSeq = seq.take(idx) ++ seq.drop(idx + 1)
//          compareAttrs(newSeq, next)
//
//  // TODO: make tail-recursive
//  def tandemTraverse(skeleton: Tree.Node, data: scala.xml.Node): Seq[Any] = {
////    def attrsToSeq(metadata: scala.xml.MetaData): Seq[Tree.Attribute] =
////      metadata match
////        case scala.xml.Null =>
////          Nil
////        case scala.xml.PrefixedAttribute(pre, key, value1, next) =>
////          val value = value1.map()
////          attrsToSeq(next).prepended(Tree.Attribute(s"$pre:$key", value))
////        case scala.xml.UnprefixedAttribute(key, value, next) =>
////          val Tree.Text(valueStr) = value
////          attrsToSeq(next).prepended(Tree.Attribute(key, valueStr))
//
//    val scala.xml.Node(dataLabel, dataAttrs1, dataChildren) = data
//    val dataAttrs = attrsToSeq(dataAttrs1)
//
//    (skeleton, data) match {
//      case (Tree.Elem(name, attrs1, children1, _), _: scala.xml.Elem) =>
//        if name == label && attrs1 == attrs2 || attrs1.isEmpty && attrs2 == null then
//          children1
//            .zip(children2)
//            .flatMap { case (s, d) => tandemTraverse(s, d) }
//        else
//          println(s"$name ==? $label, $attrs1 ==? $attrs2")
//          println(s"${attrs1.getClass} ${attrs2.getClass}")
//          ???
//
//      case (Tree.Placeholder(id), node) =>
//        Seq(node)
//    }
//  }

  def implUnapply(xmlStrCtxExpr: Expr[XML.StringContext], elemExpr: Expr[scala.xml.Node | scala.xml.NodeBuffer], scope: Expr[Scope])(using qctx: Quotes): Expr[Option[Seq[Any]]] = {
    val '{ xml(StringContext(${Varargs(parts)}: _*)) } = xmlStrCtxExpr
    val (xmlStr, offsets) = encode(parts)

//    def cf(using Scope) = ???
//    implicit val ctx: XmlContext = new XmlContext(Seq(), scope)
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

    val parsed = {
      import Parse.{apply => parse}
      import Transform.{apply => transform}
      import Validate.{apply => validate}
      import TypeCheck.{apply => typecheck}
      import Expand.{apply => expand}

      println("================")
      println(xmlStr)

      val parsed = parse(xmlStr)
      println("- parse")
      println(parsed)

      parsed

      // assert single element
//      val Seq(p) = parsed

//      given XmlContext = new XmlContext(, scope = ???)

//      expand(parsed)
    }
//      expand(parsed)

    import scala.quoted.ToExpr.SeqToExpr

    given ToExpr[Tree.Attribute] with
      def apply(attr: Tree.Attribute)(using Quotes): Expr[Tree.Attribute] =
        val Tree.Attribute(name, value) = attr
        val valueExpr = Expr[Seq[Tree.Node]](value)
        '{ Tree.Attribute(${ Expr(name) }, ${ valueExpr }) }

    given ToExpr[Tree.Node] with
      def apply(node: Tree.Node)(using Quotes): Expr[Tree.Node] =
        node match {
          case Tree.Elem(name, attrs, children, end) =>
            '{ Tree.Elem(${ Expr(name) }, ${ Expr(attrs) }, ${ Expr(children) }, ${ Expr(end) }) }
          case Tree.Placeholder(id) =>
            '{ Tree.Placeholder(${ Expr(id) }) }
        }

    import FillPlaceholders.{apply => fill_placeholders}

    '{
      ${elemExpr} match {
        case e: scala.xml.Node =>
//          given Quotes = ${ Expr(qctx) }
//          val q = summon[Quotes]
          for {
            map <- fill_placeholders(${ Expr(parsed) }, Seq(e))//(${ Expr(qctx) })
          } yield {
            val keys = map.keys.toList.sorted
            assert(!keys.zipWithIndex.exists((k, i) => k == i))
            keys.map(map)
          }
        case e =>
          println("???")
          println(e.getClass)
          ???
      }
    }

//    '{
//      ${elemExpr} match {
//        case e: Tree.Node => Some(${Expr(tandemTraverse(parsed, e))})
//        case _ => ???
//      }

//      ${implCore(xmlStr)} match {
//        case scala.xml.Node(label, attrs, child) => Some(Seq(label, attrs, child))
//        case _ => ???
////        case scala.xml.NodeBuffer(_*) => ???
//      }
//    }
//    ???
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

//    val interpolate = (
//      parse
//        andThen transform
//        andThen validate
//        andThen typecheck
//        andThen expand
//    )
//
//    interpolate(xmlStr)

    val x = xmlStr
    println("  == implCore ==")
    println(x)
    println("> parse <")
    var x1 = parse(x)
    println(x1)
    println("> transform <")
    x1 = transform(x1)
    println(x1)
    println("> validate <")
    x1 = validate(x1)
    println(x1)
    println("> typecheck <")
    x1 = typecheck(x1)
    println(x1)
    println("> expand <")
    val x2 = expand(x1)
    println(x2.show)
    x2
  }

  private def encode(parts: Seq[Expr[String]])(using Quotes): (String, Array[Int]) = {
    val sb = new StringBuilder()
    val bf = ArrayBuffer.empty[Int]

    def appendPart(part: Expr[String]) = {
      bf += sb.length
      sb ++= part.valueOrError
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
