package dotty.xml.interpolator

import scala.quoted._

import dotty.xml.interpolator.Tree._

object ValidateAttribute {

  def apply(nodes: Seq[Node])(implicit reporter: Reporter): Unit = {
    nodes.foreach {
      case Elem(_, attributes, _, children) =>
        attributes
          .groupBy(_.name)
          .collect { case (_, attributes) if attributes.size > 1 => attributes.head }
          .foreach { attribute =>
            reporter.error(
              s"attribute ${attribute.name} may only be defined once",
              attribute.pos.asInstanceOf[scala.util.parsing.input.OffsetPosition].offset
          )}
        apply(children)
      case _ =>
    }
  }
}