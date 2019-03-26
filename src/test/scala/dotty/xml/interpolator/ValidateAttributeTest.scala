package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import quoted._
import Tree._

class ValidateAttributeTest {

  @Test(expected = classOf[QuoteError]) def validate(): Unit = {
    ValidateAttribute(Seq(Elem("foo", Seq(Attribute("bar", Seq(Text("baz"))), Attribute("bar", Seq(Text("baz")))), empty = true, Nil)))
  }

}