package dotty.xml.interpolator.internal

import dotty.xml.interpolator.{xml, apply, unapplySeq}

object Debug {
  @main
  def main: Unit = {
    given scope: dotty.xml.interpolator.Scope = new dotty.xml.interpolator.Scope(null, null, null)

    val k = <foo>5</foo>
    println(k.getClass)
    val scala.xml.Node(a,b,c) = k
    println(a.getClass)
    println(b.getClass)
    println(c.getClass)
    return


//    val y = xml"<foo>${69}</foo>"
//    println(y.getClass)

    val xml"<foo>${x}</foo>" = <foo>5</foo>
    println(x)
  }
}
