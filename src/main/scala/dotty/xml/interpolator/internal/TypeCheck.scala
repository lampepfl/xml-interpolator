package dotty.xml.interpolator
package internal

import scala.quoted._

import dotty.xml.interpolator.internal.Tree._

object TypeCheck {
  def apply(nodes: Seq[Node])(using XmlContext, Reporter, Quotes): Seq[Node] = {
    typecheck(nodes)
    nodes
  }

  private def typecheck(nodes: Seq[Node])(using XmlContext, Reporter)(using Quotes): Unit = {
    import quotes.reflect._
    nodes.foreach {
      case elem : Elem =>
        elem.attributes.foreach(attribute =>
          attribute.value match {
            case Seq(Placeholder(id)) =>
              val dummy = '{ _root_.scala.xml.TopScope }
              val expr = summon[XmlContext].args(id)
              val term = Expr.betaReduce('{$expr(using $dummy)}).asTerm
              val expected = attribute.isNamespace match {
                case true => Seq(TypeRepr.of[String])
                case _ => Seq(
                  TypeRepr.of[String],
                  TypeRepr.of[collection.Seq[scala.xml.Node]],
                  TypeRepr.of[Option[collection.Seq[scala.xml.Node]]]
                )
              }
              if (!expected.exists(term.tpe <:< _)) {
                summon[Reporter].error(
                  s"""type mismatch;
                    | found   : ${term.tpe.widen.show}
                    | required: ${expected.map(_.show).mkString(" | ")}
                  """.stripMargin, term.asExpr)
              }
            case _ =>
        })
        typecheck(elem.children)
      case _ =>
    }
  }
}
