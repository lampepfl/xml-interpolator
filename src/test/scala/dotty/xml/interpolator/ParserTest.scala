package dotty.xml.interpolator

import org.junit.{Before, Test}
import org.junit.Assert._

import Tree._

class ParserTest {

  val xml: Parser = new Parser()

  @Test def parseElement(): Unit = {
    assertEquals(Elem("foo", Nil, empty = true, Nil), xml.parse(xml.Element, "<foo/>").get)
    assertEquals(Elem("foo", Nil, empty = false, Seq(Text("bar"))), xml.parse(xml.Element, "<foo>bar</foo>").get)
    assertEquals(Elem("foo", Seq(Attribute("bar", Seq(Text("baz")))), empty = false, Seq(Text("qux"))), xml.parse(xml.Element, "<foo bar='baz'>qux</bar>").get)
  }

  @Test def parseCharData(): Unit = {
    assertEquals(Text("foo"), xml.parse(xml.CharData, "foo").get)
  }

  @Test def parseElemPattern(): Unit = {
    assertEquals(Elem("foo", Nil, empty = true, Nil), xml.parse(xml.ElemPattern, "<foo/>").get)
    assertEquals(Elem("foo", Nil, empty = false, Seq(Text("bar"))), xml.parse(xml.ElemPattern, "<foo>bar</foo>").get)
  }

  @Test def parseEntityRef(): Unit = {
    assertEquals(EntityRef("foo"), xml.parse(xml.EntityRef, "&foo;").get)
  }

  @Test def parseCharRef(): Unit = {
    assertEquals(Text("&#12345;"), xml.parse(xml.CharRef, "&#12345;").get)
    assertEquals(Text("&#x0aF;"), xml.parse(xml.CharRef, "&#x0aF;").get)
  }

  @Test def parseCDSect(): Unit = {
    assertEquals(PCData("foo"), xml.parse(xml.CDSect, "<![CDATA[foo]]>").get)
  }

  @Test def parsePI(): Unit = {
    assertEquals(ProcInstr("foo", "bar"), xml.parse(xml.PI, "<?foo bar?>").get)
  }

  @Test def parseComment(): Unit = {
    assertEquals(Comment("foo"), xml.parse(xml.Comment, "<!--foo-->").get)
  }
}