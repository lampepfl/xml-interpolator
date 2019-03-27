package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import Interpolator._

class NamespaceTest {

  @Test def unprefixed(): Unit = {
    assertEquals(xml"""<foo xmlns="uri"/>""", <foo xmlns="uri"/>)
  }

  @Test def prefixed(): Unit = {
    assertEquals(xml"""<foo xmlns:pre="uri"/>""", <foo xmlns:pre="uri"/>)
  }

  @Test def multiple(): Unit = {
    assertEquals(xml"""<foo xmlns:a="uri1" xmlns:b="uri2"/>""", <foo xmlns:a="uri1" xmlns:b="uri2"/>)
  }

  @Test def nested(): Unit = {
    assertEquals(xml"""<foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>""", <foo xmlns:pre1="uri1"><bar xmlns:pre2="uri2"/></foo>)
  }

  @Test def shadowed(): Unit = {
    assertEquals(xml"""<foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>""", <foo xmlns:pre="a"><bar xmlns:pre="b"/></foo>)
  }

  @Test def scope(): Unit = {
    
  }

  @Test def invalid(): Unit = {

  }
}