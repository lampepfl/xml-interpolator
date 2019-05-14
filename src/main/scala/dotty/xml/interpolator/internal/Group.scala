package dotty.xml.interpolator.internal

object Group {
  def apply(nodes: Seq[Tree.Node]): Seq[Tree.Node] = {
    nodes.map {
      case elem : Tree.Elem =>
        val children = apply(elem.children)
        if (elem.name == "xml:group" && !elem.empty) Tree.Group(elem.children).setPos(elem.pos)
        else elem.copy(children = children)
      case node => node
    }
  }
}