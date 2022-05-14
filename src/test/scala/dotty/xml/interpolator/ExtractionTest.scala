package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

def assertThrows[E <: Throwable](code: => Unit)(using m: reflect.Manifest[E]): Unit = {
  try
    code
  catch
    case _: E => return
    case e => throw AssertionError(s"expected exception of class $m but code threw ${e.getClass}")

  throw AssertionError("code didn't throw an exception")
}

class ExtractionTest {
  @Test def elem1(): Unit = {
    val xml"<foo>$x</foo>" = <foo>{2}</foo>
    assertEquals(x, new scala.xml.Atom(2))
  }

  @Test def elem2(): Unit = {
    val xml"<foo>$x</foo>" = <foo>2</foo>
    assertEquals(x, new scala.xml.Text("2"))
  }

  @Test def elem3(): Unit = {
    val xml"<foo><bar>$x</bar></foo>" = <foo><bar>{2}</bar></foo>
    assertEquals(x, new scala.xml.Atom(2))
  }

  @Test def elem4(): Unit = {
    val xml"<foo><bar>$x</bar><baz>$y</baz></foo>" = <foo><bar>{1}</bar><baz>{2}</baz></foo>
    assertEquals(x, new scala.xml.Atom(1))
    assertEquals(y, new scala.xml.Atom(2))
  }

  @Test def buffer1(): Unit = {
    val xml"<foo/><bar/>" = <foo/><bar/>
  }

  @Test def buffer2(): Unit = {
    assertThrows[MatchError] {
      val xml"<foo/><bar/>" = <bar/><foo/>
    }
  }

  @Test def buffer3(): Unit = {
    val xml"<foo>$x</foo><bar>$y</bar>" = <foo>{1}</foo><bar>{2}</bar>
    assertEquals(x, new scala.xml.Atom(1))
    assertEquals(y, new scala.xml.Atom(2))
  }

// fixed-value matching doesn't work (it also has problems in scala2)
//  @Test def elem2(): Unit = {
//    val xml"<foo>${"bar"}</foo>" = <foo>{"bar"}</foo>
//  }

  @Test def closing1(): Unit = {
    val xml"<foo/>" = <foo/>
  }

  @Test def closing2(): Unit = {
    val xml"<foo></foo>" = <foo></foo>
  }

  @Test def closing3(): Unit = {
    val xml"<foo/>" = <foo></foo>
  }

  @Test def closing4(): Unit = {
    val xml"<foo></foo>" = <foo/>
  }

  @Test def attribute1(): Unit = {
    val xml"<foo/>" = <foo attr="val"/>
  }

  @Test def attribute2(): Unit = {
    val xml"""<foo attr="val"/>""" = <foo attr="val"/>
  }

  @Test def attribute3(): Unit = {
    val xml"""<foo attr1="val1" attr2="val2"/>""" = <foo attr1="val1" attr2="val2"/>
  }

  @Test def attribute4(): Unit = {
    val xml"""<foo attr2="val2" attr1="val1"/>""" = <foo attr1="val1" attr2="val2"/>
  }

  @Test def attribute5(): Unit = {
    val xml"""<foo attr1="val1"/>""" = <foo attr1="val1" attr2="val2"/>
  }

  @Test def attribute6(): Unit = {
    val xml"""<foo attr2="val2"/>""" = <foo attr1="val1" attr2="val2"/>
  }

  @Test def attribute7(): Unit = {
    assertThrows[MatchError] {
      val xml"""<foo attr="val"/>""" = <foo/>
    }
  }

  @Test def attribute8(): Unit = {
    assertThrows[MatchError] {
      val xml"""<foo attr="bar"/>""" = <foo attr="baz"/>
    }
  }

  @Test def attribute9(): Unit = {
    assertThrows[MatchError] {
      val xml"""<foo attr1="val"/>""" = <foo attr2="val"/>
    }
  }

  @Test def attribute10(): Unit = {
    val xml"""<foo attr=$a/>""" = <foo attr="val"/>
    assertEquals(a, scala.xml.Text("val"))
  }

  @Test def attribute11(): Unit = {
    val xml"""<foo attr1=$a attr2=$b/>""" = <foo attr2="baz" attr1="bar"/>
    assertEquals(a, scala.xml.Text("bar"))
    assertEquals(b, scala.xml.Text("baz"))
  }

  @Test def attribute12(): Unit = {
    val xml"""<foo attr=$a/>""" = <foo attr={<bar/>}/>
    assertEquals(a, <bar/>)
  }

  @Test def nested1(): Unit = {
    val xml"""<foo>${xml"<bar/>"}</foo>""" = <foo><bar/></foo>
  }

  @Test def nested2(): Unit = {
    assertThrows[MatchError] {
      val xml"""<foo>${xml"<bar/>"}</foo>""" = <foo><baz/></foo>
    }
  }

  @Test def nested3(): Unit = {
    val xml"""<foo>${xml"<bar>$x</bar>"}</foo>""" = <foo><bar>{2}</bar></foo>
    assertEquals(x, new scala.xml.Atom(2))
  }

  @Test def iterable1(): Unit = {
    val xml"<foo>$x$y</foo>" = <foo>{Seq(1, 2)}</foo>
    assertEquals(x, new scala.xml.Atom(1))
    assertEquals(y, new scala.xml.Atom(2))
  }

  // TODO: the space between $x and $y fails to match
//  @Test def iterable2(): Unit = {
//    val xml"<foo>$x $y</foo>" = <foo>{Seq(1, 2)}</foo>
//    assertEquals(x, new scala.xml.Atom(1))
//    assertEquals(y, new scala.xml.Atom(2))
//  }

  // TODO: namespace matching
//  @Test def namespace1(): Unit = {
//    val xml"""<foo xmlns=$x/>""" = <foo xmlns={"bar"}/>
//    assertEquals(x, "bar")
//  }
}
