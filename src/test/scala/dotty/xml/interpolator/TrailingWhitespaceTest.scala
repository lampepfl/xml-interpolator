package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import Interpolator._
import Utils._

class TrailingWhitespaceTest {

  @Test def discard(): Unit = {
    assert(xml" <foo/>" ≈ <foo/>)
  }

  @Test def keep1(): Unit = {
    assert(xml"<foo>   </foo>" ≈ <foo>   </foo>)
  }

  @Test def keep2(): Unit = {
    assert(xml"<foo> <bar/> </foo>" ≈ <foo> <bar/> </foo>)
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
    
    assert(expected ≈ obtained)
  }
}