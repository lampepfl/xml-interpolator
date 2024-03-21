package dotty.xml.interpolator
package internal

import Tree.*

object Validate:
  def apply(nodes: Seq[Node])(using Reporter): Seq[Node] =
    mismatchedElements(nodes)
    duplicateAttributes(nodes)
    nodes

  private def mismatchedElements(nodes: Seq[Node])(using Reporter): Unit =
    nodes.foreach:
      case elem@Elem(name, _, _, Some(end)) =>
        if name != end then reporter.error(s"closing tag `$name` expected but `$end` found", elem.pos)
        mismatchedElements(elem.children)
      case _ =>
  end mismatchedElements

  private def duplicateAttributes(nodes: Seq[Node])(using Reporter): Unit =
    nodes.foreach:
      case Elem(_, attributes, children, _) =>
        attributes
          .groupBy(_.name)
          .collect { case (_, attributes) if attributes.size > 1 => attributes.tail.head }
          .foreach { attribute => reporter.error(s"attribute `${attribute.name}` may only be defined once", attribute.pos) }
        duplicateAttributes(children)
      case _ =>
  end duplicateAttributes

end Validate