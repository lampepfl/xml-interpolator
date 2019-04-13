package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import scala.testing._

import dotty.xml.interpolator.Interpolator._

class InterpolationTest {

  @Test def elem1(): Unit = {
    assert(xml"<foo>${2}</foo>" ≈ <foo>{2}</foo>)
  }

  @Test def elem2(): Unit = {
    assert(xml"""<foo>${"bar"}</foo>""" ≈ <foo>{"bar"}</foo>)
  }

  @Test def elem3(): Unit = {
    assert(xml"<foo>2</foo>" !≈ <foo>{2}</foo>)
  }

  @Test def elem4(): Unit = {
    assert(xml"<foo>${1}${2}</foo>" ≈ <foo>{1}{2}</foo>)
  }

  @Test def elem5(): Unit = {
    assert(xml"""<foo>${xml"<bar/>"}</foo>""" ≈ <foo>{<bar/>}</foo>)
  }

  @Test def elem6(): Unit = {
    assert(xml"""<foo>${xml"<bar/><baz/>"}</foo>""" ≈ <foo>{<bar/><baz/>}</foo>)
  }

  @Test def attribute1(): Unit = {
    assert(xml"""<foo a=${"bar"}/>""" ≈ <foo a={"bar"}/>)
  }

  @Test def attribute2(): Unit = {
    assert(xml"""<foo a=${xml"<bar/>"}/>""" ≈ <foo a={<bar/>}/>)
  }

  @Test def attribute3(): Unit = {
    assert(xml"""<foo a=${xml"<bar/><baz/>"}/>""" ≈ <foo a={<bar/><baz/>}/>)
  }

  @Test def attribute4(): Unit = {
    assert(xml"""<foo a=${None}/>""" ≈ <foo a={None}/>)
  }

  @Test def attribute5(): Unit = {
    assert(!typeChecks(""" xml"<foo a=${1}/>" """))
  }

  @Test def iterable(): Unit = {
    assert(xml"<foo>${List(1, 2)}</foo>" ≈ <foo>{List(1, 2)}</foo>)
  }

  @Test def nested(): Unit = {
    assert(xml"""<foo>${xml"<bar>${1}</bar>"}</foo>""" ≈ <foo>{<bar>{1}</bar>}</foo>)
  }

  @Test def unit(): Unit = {
    assert(xml"<foo>${}</foo>" ≈ <foo>{}</foo>)
  }

  @Test def namespace1(): Unit = {
    assert(xml"""<foo xmlns=${"bar"}/>""" ≈ <foo xmlns={"bar"}/>)
  }

  @Test def namespace2(): Unit = {
    assert(!typeChecks(""" xml"<foo xmlns=${<b/>}/>" """))
  }

  @Test def namespace3(): Unit = {
    assert(!typeChecks(""" xml"<foo xmlns=${None}/>" """))
  }
}