package dotty.xml.interpolator

import scala.quoted._

import Tree._

object ValidateAttribute extends Function1[Seq[Node], Seq[Node]] {

  def apply(nodes: Seq[Node]): Seq[Node] = {
    validate(nodes)
    nodes
  }

  private def validate(nodes: Seq[Node]): Unit = {
    nodes.foreach(validate)
  }

  private def validate(node: Node): Unit = {
    node match {
      case Elem(name, attributes, empty, children) =>
        val duplicates = attributes.groupBy(_.name).collect { case (_, atts) if atts.size > 1 => atts.head }
        duplicates.foreach { duplicate => throw new QuoteError(s"duplicated attribute ${duplicate}") }
        validate(children)
      case _ =>
    }
  }
}