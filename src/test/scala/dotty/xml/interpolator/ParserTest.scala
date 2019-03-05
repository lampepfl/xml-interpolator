package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import Tree._

class ParserTest {

  private val xml: Parser = new Parser()

  @Test def XmlExpr(): Unit = {
    assertEquals(Seq(PCData("foo")), xml.parseAll(xml.XmlExpr, "<![CDATA[foo]]>").get)
    assertEquals(Seq(ProcInstr("foo", "bar")), xml.parseAll(xml.XmlExpr, "<?foo bar?>").get)
    assertEquals(Seq(Comment("foo")), xml.parseAll(xml.XmlExpr, "<!--foo-->").get)
    assertEquals(Seq(Elem("foo", Nil, empty = true, Nil)), xml.parseAll(xml.XmlExpr, "<foo/>").get)
    assertEquals(Seq(Elem("foo", Nil, empty = false, Nil)), xml.parseAll(xml.XmlExpr, "<foo></foo>").get)
    assertEquals(Seq(Elem("foo", Seq(Attribute("bar", Seq(Text("baz")))), empty = false, Seq(Text("qux")))), xml.parseAll(xml.XmlExpr, "<foo bar='baz'>qux</bar>").get)
    assertTrue(!xml.parseAll(xml.XmlExpr, "").successful)
    assertTrue(!xml.parseAll(xml.XmlExpr, "<![CDATA[foo]]>bar").successful)
    assertTrue(!xml.parseAll(xml.XmlExpr, "<?foo bar?>baz").successful)
    assertTrue(!xml.parseAll(xml.XmlExpr, "<!--foo-->bar").successful)
    assertTrue(!xml.parseAll(xml.XmlExpr, "<foo/>bar").successful)
    assertTrue(!xml.parseAll(xml.XmlExpr, "<foo></foo>bar").successful)
  }

  @Test def parseElement(): Unit = {
    assertEquals(Elem("foo", Nil, empty = true, Nil), xml.parseAll(xml.Element, "<foo/>").get)
    assertEquals(Elem("foo", Nil, empty = false, Seq(Text("bar"))), xml.parseAll(xml.Element, "<foo>bar</foo>").get)
    assertEquals(Elem("foo", Seq(Attribute("bar", Seq(Text("baz")))), empty = false, Seq(Text("qux"))), xml.parseAll(xml.Element, "<foo bar='baz'>qux</bar>").get)
  }

  @Test def parseContent(): Unit = {
    assertEquals(Seq(), xml.parseAll(xml.Content, "").get)
    assertEquals(Seq(Text("foo")), xml.parseAll(xml.Content, "foo").get)
    assertEquals(Seq(Text("foo"), Elem("bar", Nil, empty = true, Nil)), xml.parseAll(xml.Content, "foo<bar/>").get)
    assertEquals(Seq(Text("foo"), Elem("bar", Nil, empty = true, Nil), Text("baz")), xml.parseAll(xml.Content, "foo<bar/>baz").get)
    assertEquals(Seq(Text("foo"), Elem("bar", Nil, empty = true, Nil), Text("baz"), Elem("qux", Nil, empty = false, Nil)), xml.parseAll(xml.Content, "foo<bar/>baz<qux></qux>").get)
  }

  @Test def parseContent1(): Unit = {
    assertEquals(PCData("foo"), xml.parseAll(xml.Content1, "<![CDATA[foo]]>").get)
    assertEquals(ProcInstr("foo", "bar"), xml.parseAll(xml.Content1, "<?foo bar?>").get)
    assertEquals(Comment("foo"), xml.parseAll(xml.Content1, "<!--foo-->").get)
    assertEquals(Elem("foo", Seq(Attribute("bar", Seq(Text("baz")))), empty = true, Nil), xml.parseAll(xml.Content1, "<foo bar='baz'/>").get)
    assertEquals(Elem("foo", Seq(Attribute("bar", Seq(Text("baz")))), empty = false, Nil), xml.parseAll(xml.Content1, "<foo bar='baz'></foo>").get)
    assertEquals(EntityRef("foo"), xml.parseAll(xml.Content1, "&foo;").get)
    assertEquals(Text("&#12345;"), xml.parseAll(xml.Content1, "&#12345;").get)
  }

  @Test def parseXmlContent(): Unit = {
    assertEquals(PCData("foo"), xml.parseAll(xml.XmlContent, "<![CDATA[foo]]>").get)
    assertEquals(ProcInstr("foo", "bar"), xml.parseAll(xml.XmlContent, "<?foo bar?>").get)
    assertEquals(Comment("foo"), xml.parseAll(xml.XmlContent, "<!--foo-->").get)
    assertEquals(Elem("foo", Seq(Attribute("bar", Seq(Text("baz")))), empty = true, Nil), xml.parseAll(xml.XmlContent, "<foo bar='baz'/>").get)
    assertEquals(Elem("foo", Seq(Attribute("bar", Seq(Text("baz")))), empty = false, Nil), xml.parseAll(xml.XmlContent, "<foo bar='baz'></foo>").get)
    assertTrue(!xml.parseAll(xml.XmlContent, "").successful)
    assertTrue(!xml.parseAll(xml.XmlContent, "foo").successful)
  }

  @Test def parseAttribute(): Unit = {
    assertEquals(Attribute("foo", Seq(Text(""))), xml.parseAll(xml.Attribute, "foo=\"\"").get)
    assertEquals(Attribute("foo", Seq(Text("bar"))), xml.parseAll(xml.Attribute, "foo=\"bar\"").get)
    assertEquals(Attribute("foo", Seq(Text("bar"))), xml.parseAll(xml.Attribute, "foo=\'bar\'").get)
    assertEquals(Attribute("foo", Seq(Text("bar"))), xml.parseAll(xml.Attribute, "foo =\"bar\"").get)
    assertEquals(Attribute("foo", Seq(Text("bar"))), xml.parseAll(xml.Attribute, "foo= \"bar\"").get)
    assertEquals(Attribute("foo", Seq(Text("bar"))), xml.parseAll(xml.Attribute, "foo = \"bar\"").get)
    assertEquals(Attribute("foo", Seq(Text("&#12345;"))), xml.parseAll(xml.Attribute, "foo=\"&#12345;\"").get)
    assertEquals(Attribute("foo", Seq(Text("&#x0aF;"))), xml.parseAll(xml.Attribute, "foo=\"&#x0aF;\"").get)
    assertEquals(Attribute("foo", Seq(Text("bar&#12345;&#x0aF;"))), xml.parseAll(xml.Attribute, "foo=\"bar&#12345;&#x0aF;\"").get)
    assertTrue(!xml.parseAll(xml.Attribute, "foo").successful)
    assertTrue(!xml.parseAll(xml.Attribute, "foo=").successful)
    assertTrue(!xml.parseAll(xml.Attribute, "foo=bar").successful)
    assertTrue(!xml.parseAll(xml.Attribute, "foo=\"").successful)
    assertTrue(!xml.parseAll(xml.Attribute, "foo=\'").successful)
  }

  @Test def parseAttValue(): Unit = {
    assertEquals(Seq(Text("")), xml.parseAll(xml.AttValue, "\"\"").get)
    assertEquals(Seq(Text("foo")), xml.parseAll(xml.AttValue, "\"foo\"").get)
    assertEquals(Seq(Text("foo")), xml.parseAll(xml.AttValue, "\'foo\'").get)
    assertEquals(Seq(Text("&#12345;")), xml.parseAll(xml.AttValue, "\"&#12345;\"").get)
    assertEquals(Seq(Text("&#x0aF;")), xml.parseAll(xml.AttValue, "\"&#x0aF;\"").get)
    assertEquals(Seq(Text("foo&#12345;&#x0aF;")), xml.parseAll(xml.AttValue, "\"foo&#12345;&#x0aF;\"").get)
    assertTrue(!xml.parseAll(xml.AttValue, "\"").successful)
    assertTrue(!xml.parseAll(xml.AttValue, "\"foo").successful)
    assertTrue(!xml.parseAll(xml.AttValue, "\'").successful)
    assertTrue(!xml.parseAll(xml.AttValue, "\'foo").successful)
    assertTrue(!xml.parseAll(xml.AttValue, "\"<\"").successful)
    assertTrue(!xml.parseAll(xml.AttValue, "\"<foo\"").successful)
    assertTrue(!xml.parseAll(xml.AttValue, "\"&\"").successful)
    assertTrue(!xml.parseAll(xml.AttValue, "\"&foo\"").successful)
  }

  @Test def parseCharData(): Unit = {
    assertEquals(Text("foo"), xml.parseAll(xml.CharData, "foo").get)
    assertTrue(!xml.parseAll(xml.CharData, "<").successful)
    assertTrue(!xml.parseAll(xml.CharData, "&").successful)
  }

  @Test def parseChar1(): Unit = {
    assertEquals("f", xml.parseAll(xml.Char1, "f").get)
    assertTrue(!xml.parseAll(xml.Char1, "<").successful)
    assertTrue(!xml.parseAll(xml.Char1, "&").successful)
  }

  @Test def parseCharQ(): Unit = {
    assertEquals("f", xml.parseAll(xml.CharQ, "f").get)
    assertTrue(!xml.parseAll(xml.CharQ, "\"").successful)
    assertTrue(!xml.parseAll(xml.CharQ, "<").successful)
    assertTrue(!xml.parseAll(xml.CharQ, "&").successful)
  }

  @Test def parseCharA(): Unit = {
    assertEquals("f", xml.parseAll(xml.CharA, "f").get)
    assertTrue(!xml.parseAll(xml.CharA, "\'").successful)
    assertTrue(!xml.parseAll(xml.CharA, "<").successful)
    assertTrue(!xml.parseAll(xml.CharA, "&").successful)
  }

  @Test def parseXmlPattern(): Unit = {
    assertEquals(Elem("foo", Nil, empty = false, Seq(Text("bar"))), xml.parseAll(xml.ElemPattern, "<foo>bar</foo>").get)
    assertEquals(Elem("foo", Nil, empty = false, Seq(Elem("bar", Nil, empty = true, Seq()))), xml.parseAll(xml.ElemPattern, "<foo><bar/></foo>").get)
    assertEquals(Elem("foo", Nil, empty = false, Seq(Elem("bar", Nil, empty = true, Seq()), Elem("baz", Nil, empty = true, Seq()))), xml.parseAll(xml.ElemPattern, "<foo><bar/><baz/></foo>").get)
    assertEquals(Elem("foo", Nil, empty = false, Seq(Elem("bar", Nil, empty = false, Seq()))), xml.parseAll(xml.ElemPattern, "<foo><bar></bar></foo>").get)
    assertEquals(Elem("foo", Nil, empty = false, Seq(Elem("bar", Nil, empty = false, Seq()), Text("qux"))), xml.parseAll(xml.ElemPattern, "<foo><bar></bar>qux</foo>").get)
    assertEquals(Elem("foo", Nil, empty = false, Seq(Elem("bar", Nil, empty = false, Seq()), Elem("baz", Nil, empty = false, Seq()))), xml.parseAll(xml.ElemPattern, "<foo><bar></bar><baz></baz></foo>").get)
  }

  @Test def parseElemPattern(): Unit = {
    assertEquals(Elem("foo", Nil, empty = true, Nil), xml.parseAll(xml.ElemPattern, "<foo/>").get)
    assertEquals(Elem("foo", Nil, empty = false, Seq()), xml.parseAll(xml.ElemPattern, "<foo></foo>").get)
    assertTrue(!xml.parseAll(xml.ElemPattern, "<foo").successful)
    assertTrue(!xml.parseAll(xml.ElemPattern, "<foo>").successful)
    assertTrue(!xml.parseAll(xml.ElemPattern, "</foo").successful)
  }

  @Test def parseEmptyElemP(): Unit = {
    assertEquals("foo", xml.parseAll(xml.EmptyElemP, "<foo/>").get)
    assertEquals("foo", xml.parseAll(xml.EmptyElemP, "<foo />").get)
    assertTrue(!xml.parseAll(xml.EmptyElemP, "</>").successful)
    assertTrue(!xml.parseAll(xml.EmptyElemP, "< />").successful)
  }

  @Test def parseSTagP(): Unit = {
    assertEquals("foo", xml.parseAll(xml.STagP, "<foo>").get)
    assertTrue(!xml.parseAll(xml.STagP, "<foo/>").successful)
    assertTrue(!xml.parseAll(xml.STagP, "<").successful)
    assertTrue(!xml.parseAll(xml.STagP, "<>").successful)
    assertTrue(!xml.parseAll(xml.STagP, "< >").successful)
  }

  @Test def parseETagP(): Unit = {
    assertEquals("foo", xml.parseAll(xml.ETagP, "</foo>").get)
    assertTrue(!xml.parseAll(xml.ETagP, "<foo>").successful)
    assertTrue(!xml.parseAll(xml.ETagP, "<").successful)
    assertTrue(!xml.parseAll(xml.ETagP, "</").successful)
    assertTrue(!xml.parseAll(xml.ETagP, "</>").successful)
    assertTrue(!xml.parseAll(xml.ETagP, "</ >").successful)
  }

  @Test def parseContentP(): Unit = {
    assertEquals(Seq(), xml.parseAll(xml.ContentP, "").get)
    assertEquals(Seq(Text("foo")), xml.parseAll(xml.ContentP, "foo").get)
    assertEquals(Seq(Text("foo"), Elem("bar", Nil, empty = true, Nil)), xml.parseAll(xml.ContentP, "foo<bar/>").get)
    assertEquals(Seq(Text("foo"), Elem("bar", Nil, empty = true, Nil), Text("baz")), xml.parseAll(xml.ContentP, "foo<bar/>baz").get)
    assertEquals(Seq(Text("foo"), Elem("bar", Nil, empty = true, Nil), Text("baz"), Elem("qux", Nil, empty = false, Nil)), xml.parseAll(xml.ContentP, "foo<bar/>baz<qux></qux>").get)
  }

  @Test def parseReference(): Unit = {
    assertEquals(EntityRef("foo"), xml.parseAll(xml.Reference, "&foo;").get)
    assertEquals(Text("&#12345;"), xml.parseAll(xml.Reference, "&#12345;").get)
  }

  @Test def parseEntityRef(): Unit = {
    assertEquals(EntityRef("foo"), xml.parseAll(xml.EntityRef, "&foo;").get)
    assertTrue(!xml.parseAll(xml.EntityRef, "&foo").successful)
    assertTrue(!xml.parseAll(xml.EntityRef, "&;").successful)
  }

  @Test def parseCharRef(): Unit = {
    assertEquals(Text("&#12345;"), xml.parseAll(xml.CharRef, "&#12345;").get)
    assertEquals(Text("&#x0aF;"), xml.parseAll(xml.CharRef, "&#x0aF;").get)
    assertTrue(!xml.parseAll(xml.CharRef, "&#foo;").successful)
    assertTrue(!xml.parseAll(xml.CharRef, "&#;").successful)
    assertTrue(!xml.parseAll(xml.CharRef, "&#xfoo;").successful)
    assertTrue(!xml.parseAll(xml.CharRef, "&#x;").successful)
  }

  @Test def parseCDSect(): Unit = {
    assertEquals(PCData("foo"), xml.parseAll(xml.CDSect, "<![CDATA[foo]]>").get)
    assertEquals(PCData("<foo>bar</foo>"), xml.parseAll(xml.CDSect, "<![CDATA[<foo>bar</foo>]]>").get)
    assertEquals(PCData(""), xml.parseAll(xml.CDSect, "<![CDATA[]]>").get)
    assertTrue(!xml.parseAll(xml.CDSect, "<![CDATA[]]>]]>").successful)
    assertTrue(!xml.parseAll(xml.CDSect, "<![CDATA[]]>]foo]>").successful)
  }

  @Test def parsePI(): Unit = {
    assertEquals(ProcInstr("foo", "bar"), xml.parseAll(xml.PI, "<?foo bar?>").get)
    assertEquals(ProcInstr("foo", ""), xml.parseAll(xml.PI, "<?foo ?>").get)
    assertEquals(ProcInstr("foo", ""), xml.parseAll(xml.PI, "<?foo?>").get)
    assertTrue(!xml.parseAll(xml.PI, "<?xml?>").successful)
    assertTrue(!xml.parseAll(xml.PI, "<?xmlfoo?>").successful)
    assertTrue(!xml.parseAll(xml.PI, "<?foo?>?>").successful)
    assertTrue(!xml.parseAll(xml.PI, "<?foo?>foo?>").successful)
  }

  @Test def parseComment(): Unit = {
    assertEquals(Comment("foo"), xml.parseAll(xml.Comment, "<!--foo-->").get)
    assertEquals(Comment(""), xml.parseAll(xml.Comment, "<!---->").get)
    assertEquals(Comment("-foo"), xml.parseAll(xml.Comment, "<!---foo-->").get)
    assertEquals(Comment("-foo-foo"), xml.parseAll(xml.Comment, "<!---foo-foo-->").get)
    assertTrue(!xml.parseAll(xml.Comment, "<").successful)
    assertTrue(!xml.parseAll(xml.Comment, "<!").successful)
    assertTrue(!xml.parseAll(xml.Comment, "<!--").successful)
    assertTrue(!xml.parseAll(xml.Comment, "<!---").successful)
    assertTrue(!xml.parseAll(xml.Comment, "<!---").successful)
    assertTrue(!xml.parseAll(xml.Comment, "<!--->").successful)
    assertTrue(!xml.parseAll(xml.Comment, "<!---foo--->").successful)
    assertTrue(!xml.parseAll(xml.Comment, "<!----->-->").successful)
  }

  @Test def parseName(): Unit = {
    assertTrue(xml.parseAll(xml.Name, "foo").successful)
    assertTrue(xml.parseAll(xml.Name, "Foo").successful)
    assertTrue(xml.parseAll(xml.Name, "_foo").successful)
    assertTrue(xml.parseAll(xml.Name, ":foo").successful)
    assertTrue(xml.parseAll(xml.Name, "foo ").successful)
    assertTrue(xml.parseAll(xml.Name, "foo1").successful)
    assertTrue(xml.parseAll(xml.Name, "foo:bar").successful)
    assertTrue(xml.parseAll(xml.Name, "foo.bar").successful)
    assertTrue(xml.parseAll(xml.Name, "foo_bar").successful)
    assertTrue(xml.parseAll(xml.Name, "foo-bar").successful)
    assertTrue(!xml.parseAll(xml.Name, "1foo").successful)
    assertTrue(!xml.parseAll(xml.Name, " foo").successful)
    assertTrue(!xml.parseAll(xml.Name, ".foo").successful)
    assertTrue(!xml.parseAll(xml.Name, "-foo").successful)
  }

  @Test def parseEq(): Unit = {
    assertTrue(xml.parseAll(xml.Eq, "=").successful)
    assertTrue(xml.parseAll(xml.Eq, " =").successful)
    assertTrue(xml.parseAll(xml.Eq, "= ").successful)
    assertTrue(xml.parseAll(xml.Eq, " = ").successful)
    assertTrue(!xml.parseAll(xml.Eq, "").successful)
  }
}