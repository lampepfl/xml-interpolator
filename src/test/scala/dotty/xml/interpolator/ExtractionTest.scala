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

  // matching against a value doesn't work (it also has problems in scala2)
//  @Test def elem5(): Unit = {
//    val xml"<foo>${"bar"}</foo>" = <foo>{"bar"}</foo>
//  }

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

  @Test def namespace1(): Unit = {
    val xml"<foo xmlns=$x/>" = <foo xmlns={"bar"}/>
    assertEquals(x, "bar")
  }

  @Test def namespace2(): Unit = {
    val xml"""<foo xmlns="bar"/>""" = <foo xmlns={"bar"}/>
  }

  @Test def namespace3(): Unit = {
    val xml"<foo/>" = <foo xmlns={"bar"}/>
  }

  @Test def namespace4(): Unit = {
    val xml"<foo xmlns:pre=$x/>" = <foo xmlns:pre={"bar"}/>
    assertEquals(x, "bar")
  }

  @Test def namespace5(): Unit = {
    val xml"""<foo xmlns:pre="bar"/>""" = <foo xmlns:pre={"bar"}/>
  }

  @Test def namespace6(): Unit = {
    val xml"<foo/>" = <foo xmlns:pre={"bar"}/>
  }

  @Test def namespace7(): Unit = {
    val xml"<foo xmlns:a=$a xmlns:b=$b/>" = <foo xmlns:a="uri1" xmlns:b="uri2"/>
    assertEquals(a, "uri1")
    assertEquals(b, "uri2")
  }

  @Test def namespace8(): Unit = {
    val xml"<foo xmlns:a=$a xmlns:b=$b/>" = <foo xmlns:b="uri2" xmlns:a="uri1"/>
    assertEquals(a, "uri1")
    assertEquals(b, "uri2")
  }

  @Test def namespace9(): Unit = {
    val xml"""<foo xmlns:a="uri1" xmlns:b="uri2"/>""" = <foo xmlns:a="uri1" xmlns:b="uri2"/>
  }

  @Test def namespace10(): Unit = {
    val xml"""<foo xmlns:a="uri1" xmlns:b="uri2"/>""" = <foo xmlns:b="uri2" xmlns:a="uri1"/>
  }

  @Test def namespace11(): Unit = {
    val xml"""<foo xmlns:a="uri1"/>""" = <foo xmlns:a="uri1" xmlns:b="uri2"/>
  }

  @Test def namespace12(): Unit = {
    val xml"""<foo xmlns:b="uri2"/>""" = <foo xmlns:a="uri1" xmlns:b="uri2"/>
  }

  @Test def namespace13(): Unit = {
    val xml"""<foo xmlns:b=$b/>""" = <foo xmlns:a="uri1" xmlns:b="uri2"/>
    assertEquals(b, "uri2")
  }

  @Test def namespace14(): Unit = {
    val xml"""<foo/>""" = <foo xmlns:a="uri1" xmlns:b="uri2"/>
  }

  @Test def namespace15(): Unit = {
    val xml"<foo xmlns:pre1=$a><bar xmlns:pre2=$b/></foo>" = <foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>
    assertEquals(a, "uri1")
    assertEquals(b, "uri2")
  }

  @Test def namespace16(): Unit = {
    val xml"<foo xmlns:pre=$a><bar xmlns:pre=$b/></foo>" = <foo xmlns:pre="uri1"><bar xmlns:pre="uri2"/></foo>
    assertEquals(a, "uri1")
    assertEquals(b, "uri2")
  }
}
