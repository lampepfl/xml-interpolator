package dotty.xml.interpolator
package internal

import scala.quoted.*

import Tree.*

object TypeCheck:

  def apply(nodes: Seq[Node])(using XmlContext, Reporter, Quotes): Seq[Node] =
    typecheck(nodes)
    nodes

  private def typecheck(nodes: Seq[Node])(using XmlContext, Reporter, Quotes): Unit =
    import quotes.reflect.*
    nodes.foreach:
      case elem : Elem =>
        elem.attributes.foreach: attribute =>
          attribute.value match
            case Seq(Placeholder(id)) =>
              val expr = ctx.args(id)
              val term = Expr.betaReduce('{$expr(using scala.xml.TopScope)}).asTerm
              val expected = attribute.isNamespace match
                case true => Seq(TypeRepr.of[String])
                case _ => Seq(
                  TypeRepr.of[String],
                  TypeRepr.of[collection.Seq[scala.xml.Node]],
                  TypeRepr.of[Option[collection.Seq[scala.xml.Node]]]
                )
              if !expected.exists(term.tpe <:< _) then
                reporter.error(
                  s"""type mismatch;
                    | found   : ${term.tpe.widen.show}
                    | required: ${expected.map(_.show).mkString(" | ")}
                  """.stripMargin, term.asExpr)
            case _ =>
        typecheck(elem.children)
      case _ =>
  end typecheck

end TypeCheck
