package dotty.xml.interpolator

import scala.quoted._

import dotty.xml.interpolator.Tree._

object ValidateAttribute {

  def apply(nodes: Seq[Node]): Unit = {
    nodes.foreach(node => node match {
      case Elem(_, attributes, _, children) =>
        val duplicates = attributes.groupBy(_.name).collect { case (_, attributes) if attributes.size > 1 => attributes.head }
        duplicates.foreach { duplicate => throw new QuoteError(s"attribute ${duplicate.name} may only be defined once") }
        apply(children)
      case _ =>
    })
  }
}