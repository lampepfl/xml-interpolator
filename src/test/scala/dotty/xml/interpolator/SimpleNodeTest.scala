package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import Interpolator._

class SimpleNodeTest {

  @Test def sequence(): Unit = {
    assertEquals(xml"<foo/><bar/>", <foo/><bar/>)
  }

  @Test def minimized(): Unit = {
    assertEquals(xml"<foo/>", <foo/>)
  }

  @Test def elem(): Unit = {
    assertEquals(xml"<foo></foo>", <foo></foo>)
  }

  @Test def nested(): Unit = {
    assertEquals(xml"<foo><bar/></foo>", <foo><bar/></foo>)
  }

  @Test def unprefixed(): Unit = {
    assertEquals(xml"""<foo a="a" b="b"/>""", <foo a="a" b="b"/>)
  }

  @Test def prefixed(): Unit = {
    assertEquals(xml"""<foo a:a="a" b:b="b"/>""", <foo a:a="a" b:b="b"/>)
  }

  @Test def attribute(): Unit = {
    assertEquals(xml"""<foo a="'"/>""", <foo a="'"/>)
    assertEquals(xml"""<foo a='"'/>""", <foo a='"'/>)
  }

  @Test def text(): Unit = {
    assertEquals(xml"<foo>bar</foo>", <foo>bar</foo>)
    assertEquals(xml"<foo>></foo>", <foo>></foo>)
    assertEquals(xml"<foo>{</foo>", <foo>{{</foo>)
    assertEquals(xml"<foo>}</foo>", <foo>}}</foo>)
  }

  @Test def entityRef(): Unit = {

  }

  @Test def charRef(): Unit = {

  }

  @Test def group(): Unit = {
    assertEquals(xml"<xml:group><foo/><bar/></xml:group>", <xml:group><foo/><bar/></xml:group>)
    assertEquals(xml"<xml:group></xml:group>", <xml:group></xml:group>)
  }

  @Test def comment(): Unit = {
    assertEquals(xml"<!---->", <!---->)
    //assertEquals(xml"<!----->", <!----->)
    assertEquals(xml"<!--foo-->", <!--foo-->)
    assertEquals(xml"<!--a-b-->", <!--a-b-->)
  }

  @Test def pcData(): Unit = {
    assertEquals(xml"<![CDATA[foo]]>", <![CDATA[foo]]>)
    assertEquals(xml"<![CDATA[]]>", <![CDATA[]]>)
    assertEquals(xml"<![CDATA[>]]>", <![CDATA[>]]>)
    assertEquals(xml"<![CDATA[]>]]>", <![CDATA[]>]]>)
    assertEquals(xml"<![CDATA[]]]]>", <![CDATA[]]]]>)
  }

  @Test def procInstr(): Unit = {
    assertEquals(xml"<?foo bar?>", <?foo bar?>)
    assertEquals(xml"<?foo?>", <?foo?>)
    assertEquals(xml"<?foo    ?>", <?foo    ?>)
    assertEquals(xml"<?foo  bar?>", <?foo  bar?>)
    //assertEquals(xml"<?foo??>", <?foo??>)
    //assertEquals(xml"<?foo<bar?>", <?foo<bar?>)
  }

  @Test def unparsed(): Unit = {
    assertEquals(xml"<xml:unparsed>foo</xml:unparsed>", <xml:unparsed>foo</xml:unparsed>)
    assertEquals(xml"<xml:unparsed>{</xml:unparsed>", <xml:unparsed>{</xml:unparsed>)
    assertEquals(xml"<xml:unparsed><</xml:unparsed>", <xml:unparsed><</xml:unparsed>)
  }

  @Test def coalescing(): Unit = {
    
  }
}