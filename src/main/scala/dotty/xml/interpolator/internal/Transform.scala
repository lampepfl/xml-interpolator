package dotty.xml.interpolator
package internal

import Tree.*

object Transform:

  def apply(nodes: Seq[Node]): Seq[Node] =
    nodes.map:
      case elem : Elem =>
        val children = apply(elem.children)
        if elem.name == "xml:group" && !elem.end.isEmpty then Group(elem.children).setPos(elem.pos)
        else elem.copy(children = children).setPos(elem.pos)
      case node => node
  end apply

end Transform