package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import Interpolator._

class InterpolationTest {

  @Test def elem(): Unit = {
    assertEquals(xml"<foo>${2}</foo>", <foo>{2}</foo>)
    assertEquals(xml"<foo>${"bar"}</foo>", <foo>{"bar"}</foo>)
    assertNotEquals(xml"<foo>2</foo>", <foo>{2}</foo>)
    assertEquals(xml"<foo>${1}${2}</foo>", <foo>{1}{2}</foo>)
    assertEquals(xml"""<foo>${xml"<bar/>"}</foo>""", <foo>{<bar/>}</foo>)
    assertEquals(xml"""<foo>${xml"<bar/><baz/>"}</foo>""", <foo>{<bar/><baz/>}</foo>)
  }

  @Test def attribute(): Unit = {
    assertEquals(xml"""<foo a=${"bar"}/>""", <foo a={"bar"}/>)
    assertEquals(xml"""<foo a=${xml"<bar/>"}/>""", <foo a={<bar/>}/>)
    assertEquals(xml"""<foo a=${xml"<bar/><baz/>"}/>""", <foo a={<bar/><baz/>}/>)
    assertEquals(xml"""<foo a=${None}/>""", <foo a={None}/>)
    // xml"<foo a=${1}/>" should not type check
  }

  @Test def iterable(): Unit = {
    assertEquals(xml"<foo>${List(1, 2)}</foo>", <foo>{List(1, 2)}</foo>)
  }

  @Test def nested(): Unit = {
    assertEquals(xml"""<foo>${xml"<bar>${1}</bar>"}</foo>""", <foo>{<bar>{1}</bar>}</foo>)
  }

  @Test def unit(): Unit = {
    //assertEquals(xml"<foo>${}</foo>", <foo>{}</foo>)
  }

  @Test def namespace(): Unit = {
    // assertEquals(xml"""<foo xmlns=${"bar"}/>""", <foo xmlns={"bar"}/>)
    // xml"""<foo xmlns=${xml"<bar/>"}/>""" should not type check
    // xml"<foo xmlns=${None}/>" should not type check
  }
}