package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import quoted._
import Tree._

class ValidateAttributeTest {

  @Test def validate(): Unit = {
    val xml = Seq(Elem("foo", Seq(Attribute("bar", Seq(Text("baz")))), empty = true, Nil))
    assertEquals(xml, ValidateAttribute(xml))
  }

  @Test(expected = classOf[QuoteError]) def validateError(): Unit = {
    ValidateAttribute(Seq(Elem("foo", Seq(Attribute("bar", Seq(Text("baz"))), Attribute("bar", Seq(Text("baz")))), empty = true, Nil)))
  }

}