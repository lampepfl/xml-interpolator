package dotty.xml.interpolator

implied NodeOps {

  def (self: scala.xml.Node) ≈ (that: scala.xml.Node): Boolean =
    self == that && hasSameScope(self, that)

  def (self: scala.xml.Node) !≈ (that: scala.xml.Node): Boolean = !(self ≈ that)

  private def hasSameScope(self: scala.xml.Node, that: scala.xml.Node): Boolean =
    self.scope == that.scope && {
      val zipped = (self, that) match {
        case (g1: scala.xml.Group, g2: scala.xml.Group) => (g1.nodes, g2.nodes).zipped
        case (n1, n2)                       => (n1.child, n2.child).zipped
      }
      zipped.forall(hasSameScope)
    }
}

implied NodeBufferOps {
  def (self: scala.xml.NodeBuffer) ≈ (that: scala.xml.NodeBuffer): Boolean = {
    val selfIt = self.iterator
    val thatIt = that.iterator

    while (selfIt.hasNext && thatIt.hasNext) {
      if (!(selfIt.next() ≈ thatIt.next())) {
        return false
      }
    }

    selfIt.isEmpty && thatIt.isEmpty
  }

  def (self: scala.xml.NodeBuffer) !≈ (that: scala.xml.NodeBuffer): Boolean = !(self ≈ that)
}
