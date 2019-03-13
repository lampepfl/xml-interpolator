package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

class EncodeHoleTest {

  @Test def encode(): Unit = {
    assertEquals("<foo/>", EncodeHole(StringContext("<foo/>")))
    assertEquals("<foo>" + Hole.HoleStart + "</foo>", EncodeHole(StringContext("<foo>", "</foo>")))
    assertEquals("<foo>" + Hole.HoleStart + "</foo>" + Hole.HoleStart + Hole.HoleChar + "<bar>", EncodeHole(StringContext("<foo>", "</foo>", "<bar>")))
  }
}