package dotty.xml.interpolator

import dotty.xml.interpolator.Tree._

object GroupElement {

  def apply(nodes: Seq[Node]): Seq[Node] = {
    nodes.map {
      case elem : Elem =>
        val children = apply(elem.children)
        if (elem.name == "xml:group" && !elem.empty) Group(elem.children).setPos(elem.pos)
        else elem.copy(children = children)
      case node => node
    }
  }
}