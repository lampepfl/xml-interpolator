package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import Interpolator._

class InterpolatorTest {
  @Test def test(): Unit = {
    val w = "world"
    assertEquals(xml"Hello $w!", "Hello world!")
  }
}