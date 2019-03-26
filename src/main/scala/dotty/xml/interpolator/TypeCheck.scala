package dotty.xml.interpolator

import scala.language.implicitConversions
import scala.quoted._
import scala.tasty._

import Tree._

object TypeCheck {

  def apply(nodes: Seq[Node], args: List[Expr[Any]])(implicit reflection: Reflection): Unit = {
    import reflection._
    nodes.foreach(node => node match {
      case e : Elem =>
        e.attributes.foreach(attribute => attribute.value match {
          case Seq(Placeholder(id)) =>
            val term = args(id).unseal
            val expected = attribute.isNamespace match {
              case true => Seq('[String].unseal.tpe)
              case _ => Seq('[String].unseal.tpe,'[Seq[scala.xml.Node]].unseal.tpe, '[Option[Seq[scala.xml.Node]]].unseal.tpe)
            }
            if (!expected.exists(term.tpe <:< _)) {
              throw new QuoteError(
                s"""type mismatch;
                  | found   : ${term.tpe}
                  | required: ${expected.mkString(" | ")}
                """.stripMargin
              )
            }
          case _ =>
        })
      case _ =>
    })
  }
}