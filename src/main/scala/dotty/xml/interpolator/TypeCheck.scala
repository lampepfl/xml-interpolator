package dotty.xml.interpolator

import scala.language.implicitConversions
import scala.quoted._
import scala.tasty._

import Tree._

class TypeCheck {

  def apply(nodes: Seq[Node], args: List[Expr[Any]])(implicit reflection: Reflection): Seq[Node] = {
    typecheck(nodes, args)
    nodes
  }

  private def typecheck(nodes: Seq[Node], args: List[Expr[Any]])(implicit reflection: Reflection): Unit = {
    nodes.foreach(typecheck(_, args))
  }

  private def typecheck(node: Node, args: List[Expr[Any]])(implicit reflection: Reflection): Unit = {
    node match {
      case Elem(name, attributes, empty, children) => attributes.foreach(typecheck(_, args))
      case _ =>
    }
  }

  private def typecheck(attribute: Attribute, args: List[Expr[Any]])(implicit reflection: Reflection): Unit = {
    import reflection._
    // TODO
    //  namespace => String
    // !namespace => String, Seq[scala.xml.Node], Option[Seq[scala.xml.Node]]
    attribute.value.foreach {
      case Placeholder(id) =>
        val term = args(id).unseal
        if (!(term.tpe <:< definitions.StringType)) {
          throw new QuoteError(
            s"""type mismatch;
              | found   : ${term.tpe}
              | required: ${definitions.StringType}
            """.stripMargin
          )
        }
      case _ =>
    }
  }
}