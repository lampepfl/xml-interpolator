package dotty.xml.interpolator.internal

import scala.quoted._
import scala.tasty._

import dotty.xml.interpolator.internal.Tree._

object TypeCheck {
  def apply(nodes: Seq[Node]) given (args: Seq[Expr[Any]]) given (reporter: Reporter) given (reflect: Reflection): Unit = {
    import reflect._
    nodes.foreach {
      case elem : Elem =>
        elem.attributes.foreach(attribute =>
          attribute.value match {
            case Seq(Placeholder(id)) =>
              val term = args(id).unseal
              val expected = attribute.isNamespace match {
                case true => Seq('[String].unseal.tpe)
                case _ => Seq(
                  '[String].unseal.tpe,
                  '[Seq[scala.xml.Node]].unseal.tpe,
                  '[Option[Seq[scala.xml.Node]]].unseal.tpe
                )
              }
              if (!expected.exists(term.tpe <:< _)) {
                reporter.error(
                  """type mismatch;
                    | found   : ${term.tpe.widen.show}
                    | required: ${expected.map(_.show).mkString(" | ")}
                  """.stripMargin, term.seal)
              }
            case _ =>
        })
        apply(elem.children)
      case _ =>
    }
  }
}