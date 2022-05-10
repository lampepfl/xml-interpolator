package dotty.xml.interpolator.internal

import dotty.xml.interpolator.{xml, apply, unapplySeq}

object Debug {
  @main
  def main: Unit = {
    given scope: dotty.xml.interpolator.Scope = new dotty.xml.interpolator.Scope(null, null, null)

//    val y = xml"<foo>${69}</foo>"
//    println(y.getClass)

//    val xml"<bar>${x}</bar>" = <bar>5</bar>
//    println(x)
//    val xml"<a><b>$a</b><c>$b</c></a>" = <a><b>1</b><c>2</c></a>
//    println(s"($a, $b)")
////    val xml"<tag o=\"$j\"></tag>" = <tag o='hmmm'></tag>
////    println(j)

    // TODO: interpolator doesn't work with attributes (open issue)
//    val k = xml"<tag o='o' k='0'>0</tag>"
//    println(k)

    // TODO: match xmlns
//    val xml"<tag xmlns:attr='a'>$x</tag>" = <tag xmlns:attr='a'>2</tag>
//    println(x)

    val xml"<tag/>" = <tag/>
    val xml"<tag/>" = <tag></tag>
    val xml"<tag></tag>" = <tag/>
    val xml"<tag></tag>" = <tag></tag>

//    val xml"<t a='v'>$x</t>" = <t a='v'>0</t>

    <hmm a=":)">42</hmm> match
      case xml"<hmm>$x</hmm>" =>
        println(s"WRONG, case 1: $x")
      case xml"<hmm a=''>$x</hmm>" =>
        println(s"WRONG, case 2: $x")
      case xml"<hmm a='a'>$x</hmm>" =>
        println(s"WRONG, case 3: $x")
      case xml"<hmm a=':) '>$x</hmm>" =>
        println(s"WRONG, case 4: $x")
      case xml"<hmm a=':)' b=''>$x</hmm>" =>
        println(s"WRONG, case 5: $x")
      case xml"<hmm a=':)'>$x</hmm>" =>
        println(s"CORRECT, case 6: $x")
      case _ =>
        println("WRONG, no case")
  }
}
