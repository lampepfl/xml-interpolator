package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import Interpolator._
import Utils._

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

  @Test def scope(): Unit = {
    assert(xml"""<foo xmlns:pre="scope0">${xml"<bar/>"}</foo>""" ≈ <foo xmlns:pre="scope0">{<bar/>}</foo>)
  }
}