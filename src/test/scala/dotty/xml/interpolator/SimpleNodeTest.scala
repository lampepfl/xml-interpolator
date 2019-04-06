package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import dotty.xml.interpolator.Interpolator._

class SimpleNodeTest {

  @Test def sequence(): Unit = {
    assert(xml"<foo/><bar/>" ≈ <foo/><bar/>)
  }

  @Test def minimized(): Unit = {
    assert(xml"<foo/>" ≈ <foo/>)
  }

  @Test def elem(): Unit = {
    assert(xml"<foo></foo>" ≈ <foo></foo>)
  }

  @Test def nested(): Unit = {
    assert(xml"<foo><bar/></foo>" ≈ <foo><bar/></foo>)
  }

  @Test def unprefixed(): Unit = {
    assert(xml"""<foo a="a" b="b"/>""" ≈ <foo a="a" b="b"/>)
  }

  @Test def prefixed(): Unit = {
    assert(xml"""<foo a:a="a" b:b="b"/>""" ≈ <foo a:a="a" b:b="b"/>)
  }

  @Test def attribute1(): Unit = {
    assert(xml"""<foo a="'"/>""" ≈ <foo a="'"/>)
  }

  @Test def attribute2(): Unit = {
    assert(xml"""<foo a='"'/>""" ≈ <foo a='"'/>)
  }

  @Test def text1(): Unit = {
    assert(xml"<foo>bar</foo>" ≈ <foo>bar</foo>)
  }

  @Test def text2(): Unit = {
    assert(xml"<foo>></foo>" ≈ <foo>></foo>)
  }

  @Test def text3(): Unit = {
    assert(xml"<foo>{</foo>" ≈ <foo>{{</foo>)
  }

  @Test def text4(): Unit = {
    assert(xml"<foo>}</foo>" ≈ <foo>}}</foo>)
  }

  @Test def entityRef1(): Unit = {
    assert(xml"<foo>&name;</foo>" ≈ <foo>&name;</foo>)
  }

  @Test def entityRef2(): Unit = {
    assert(xml"<foo>&lt;</foo>" ≈ <foo>&lt;</foo>)
  }

  @Test def entityRef3(): Unit = {
    assert(xml"<foo>Hello &name;!</foo>" ≈ <foo>Hello &name;!</foo>)
  }

  @Test def entityRef4(): Unit = {
    assert(xml"<foo>&na:me;</foo>" ≈ <foo>&na:me;</foo>)
  }

  @Test def entityRef5(): Unit = {
    // assert(xml"""<foo a="&name;"/>""" ≈ <foo a="&name;"/>)
    fail
  }

  @Test def entityRef6(): Unit = {
    // assert(xml"""<foo a="&na:me;"/>""" ≈ <foo a="&na:me;"/>)
    fail
  }

  @Test def entityRef7(): Unit = {
    // assert(xml"""<foo a="&lt;"/>""" ≈ <foo a="&lt;"/>)
    fail
  }

  @Test def entityRef8(): Unit = {
    // assert(xml"""<foo a="Hello &name;!"/>""" ≈ <foo a="Hello &name;!"/>)
    fail
  }

  @Test def entityRef9(): Unit = {
    // assert(xml"""<foo a="1 &lt; 2"/>""" ≈ <foo a="1 &lt; 2"/>)
    fail
  }

  @Test def charRef1(): Unit = {
    assert(xml"<foo>&#1234;</foo>" ≈ <foo>&#1234;</foo>)
  }

  @Test def charRef2(): Unit = {
    assert(xml"<foo>&#x1234;</foo>" ≈ <foo>&#x1234;</foo>)
  }

  @Test def charRef3(): Unit = {
    assert(xml"<foo>Hello&#x1234;World</foo>" ≈ <foo>Hello&#x1234;World</foo>)
  }

  @Test def charRef4(): Unit = {
    assert(xml"""<foo a="&#1234;"/>""" ≈ <foo a="&#1234;"/>)
  }

  @Test def charRef5(): Unit = {
    assert(xml"""<foo a="&#x1234;" />""" ≈ <foo a="&#x1234;"/>)
  }

  @Test def charRef6(): Unit = {
    assert(xml"""<foo a="Hello&#x1234;World"/>""" ≈ <foo a="Hello&#x1234;World"/>)
  }

  @Test def group1(): Unit = {
    assert(xml"<xml:group><foo/><bar/></xml:group>" ≈ <xml:group><foo/><bar/></xml:group>)
  }

  @Test def group2(): Unit = {
    assert(xml"<xml:group></xml:group>" ≈ <xml:group></xml:group>)
  }

  @Test def comment1(): Unit = {
    assert(xml"<!---->" ≈ <!---->)
  }

  @Test def comment2(): Unit = {
    assert(xml"<!----->" ≈ <!----->)
  }

  @Test def comment3(): Unit = {
    assert(xml"<!--foo-->" ≈ <!--foo-->)
  }

  @Test def comment4(): Unit = {
    assert(xml"<!--a-b-->" ≈ <!--a-b-->)
  }

  @Test def pcData1(): Unit = {
    assert(xml"<![CDATA[foo]]>" ≈ <![CDATA[foo]]>)
  }

  @Test def pcData2(): Unit = {
    assert(xml"<![CDATA[]]>" ≈ <![CDATA[]]>)
  }

  @Test def pcData3(): Unit = {
    assert(xml"<![CDATA[>]]>" ≈ <![CDATA[>]]>)
  }

  @Test def pcData4(): Unit = {
    assert(xml"<![CDATA[]>]]>" ≈ <![CDATA[]>]]>)
  }

  @Test def pcData5(): Unit = {
    assert(xml"<![CDATA[]]]]>" ≈ <![CDATA[]]]]>)
  }

  @Test def procInstr1(): Unit = {
    assert(xml"<?foo bar?>" ≈ <?foo bar?>)
  }

  @Test def procInstr2(): Unit = {
    assert(xml"<?foo?>" ≈ <?foo?>)
  }

  @Test def procInstr3(): Unit = {
    assert(xml"<?foo    ?>" ≈ <?foo    ?>)
  }

  @Test def procInstr4(): Unit = {
    assert(xml"<?foo  bar?>" ≈ <?foo  bar?>)
  }

  @Test def procInstr5(): Unit = {
    assert(xml"<?foo??>" ≈ <?foo??>)
  }

  @Test def procInstr6(): Unit = {
    assert(xml"<?foo<bar?>" ≈ <?foo<bar?>)
  }

  @Test def unparsed1(): Unit = {
    assert(xml"<xml:unparsed>foo</xml:unparsed>" ≈ <xml:unparsed>foo</xml:unparsed>)
  }

  @Test def unparsed2(): Unit = {
    assert(xml"<xml:unparsed>{</xml:unparsed>" ≈ <xml:unparsed>{</xml:unparsed>)
  }

  @Test def unparsed3(): Unit = {
    assert(xml"<xml:unparsed><</xml:unparsed>" ≈ <xml:unparsed><</xml:unparsed>)
  }
}