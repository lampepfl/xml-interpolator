package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import Interpolator._

class TrailingWhitespaceTest {

  @Test def discard(): Unit = {
    assertEquals(xml" <foo/>", <foo/>)
  }

  @Test def keep(): Unit = {
    assertEquals(xml"<foo>   </foo>", <foo>   </foo>)
    assertEquals(xml"<foo> <bar/> </foo>", <foo> <bar/> </foo>)
  }

  @Test def multiline(): Unit = {
    val expected = xml"""
      <foo>
          <bar/>
      </foo>
    """
    
    val obtained = 
      <foo>
          <bar/>
      </foo>
    
    assertEquals(expected, obtained)
  }

}