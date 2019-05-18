package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

class NamespaceTest {

  @Test def unprefixed(): Unit = {
    assert(xml"""<foo xmlns="uri"/>""" ≈ <foo xmlns="uri"/>)
  }

  @Test def prefixed(): Unit = {
    assert(xml"""<foo xmlns:pre="uri"/>""" ≈ <foo xmlns:pre="uri"/>)
  }

  @Test def multiple(): Unit = {
    assert(xml"""<foo xmlns:a="uri1" xmlns:b="uri2"/>""" ≈ <foo xmlns:a="uri1" xmlns:b="uri2"/>)
  }

  @Test def nested(): Unit = {
    assert(xml"""<foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>""" ≈ <foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>)
  }

  @Test def shadowed(): Unit = {
    assert(xml"""<foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>""" ≈ <foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>)
  }

  @Test def scope1(): Unit = {
    assert(xml"""<foo xmlns:pre="scope0">${xml"<bar/>"}</foo>""" ≈ <foo xmlns:pre="scope0">{<bar/>}</foo>)
  }

  @Test def scope2(): Unit = {
    assert(xml"""<foo xmlns:pre="scope0">${xml"<bar/>"}</foo>""" ≈ xml"""<foo xmlns:pre="scope0"><bar/></foo>""")
  }

  @Test def scope3(): Unit = {
    assert(xml"""<foo xmlns:pre="scope0">${xml"<bar><baz/></bar>"}</foo>""" ≈ <foo xmlns:pre="scope0">{<bar><baz/></bar>}</foo>)
  }

  @Test def scope4(): Unit = {
    assert(xml"""<foo xmlns:pre="scope0">${xml"""<bar>${xml"<baz/>"}</bar>"""}</foo>""" ≈ <foo xmlns:pre="scope0">{<bar>{<baz/>}</bar>}</foo>)
  }

  @Test def scope5(): Unit = {
    assert(xml"""<foo xmlns:zero="scope0">${xml"""<bar xmlns:one="scope1">${xml"""<baz xmlns:two="scope2"/>"""}</bar>"""}</foo>""" ≈ <foo xmlns:zero="scope0">{<bar xmlns:one="scope1">{<baz xmlns:two="scope2"/>}</bar>}</foo>)
  }

  @Test def scope6(): Unit = {
    val xml1 = xml"<bar/>"
    val xml2 = <bar/>
    assert(xml"""<foo xmlns:pre="scope0">${xml1}</foo>""" ≈ <foo xmlns:pre="scope0">{xml2}</foo>)
  }
}