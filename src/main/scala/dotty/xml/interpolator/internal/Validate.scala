package dotty.xml.interpolator.internal

import scala.language.implicitConversions

import dotty.xml.interpolator.internal.Tree._

object Validate {
  def apply(nodes: Seq[Node]) given Reporter: Seq[Node] = {
    validate(nodes)
    nodes
  }

  private def validate(nodes: Seq[Node]) given (reporter: Reporter): Unit = {
    nodes.foreach {
      case Elem(_, attributes, _, children) =>
        attributes
          .groupBy(_.name)
          .collect { case (_, attributes) if attributes.size > 1 => attributes.head }
          .foreach { attribute => reporter.error(s"attribute ${attribute.name} may only be defined once", attribute.pos )}
        validate(children)
      case _ =>
    }
  }
}