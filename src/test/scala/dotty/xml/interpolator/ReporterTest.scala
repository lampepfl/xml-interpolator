package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

class ReporterTest {

  inline def (inline ctx: StringContext) xml (inline args: ((given Scope) => Any)*)(given Scope) <: Any =
    ${ dotty.xml.interpolator.internal.Macro.implErrors('ctx, 'args, '{implicitly[Scope]}) }

  @Test def empty1(): Unit = {
    assertEquals(xml"", List((0, "'<' expected but end of source found")))
  }

  @Test def empty2(): Unit = {
    assertEquals(xml" ", List((1, "'<' expected but end of source found")))
  }

  @Test def garbage1(): Unit = {
    assertEquals(xml"foo", List((0, "'<' expected but 'f' found")))
  }

  @Test def garbage2(): Unit = {
    assertEquals(xml"123", List((0, "'<' expected but '1' found")))
  }

  @Test def elem1(): Unit = {
    assertEquals(xml"<>", List((1, "'_' or a letter expected but '>' found")))
  }

  @Test def elem2(): Unit = {
    assertEquals(xml"<123/>", List((1, "'_' or a letter expected but '1' found")))
  }

  @Test def elem3(): Unit = {
    assertEquals(xml"< foo/>", List((1, "'_' or a letter expected but ' ' found")))
  }

  @Test def elem4(): Unit = {
    assertEquals(xml"</foo>", List((1, "'_' or a letter expected but '/' found")))
  }

  @Test def elem5(): Unit = {
    assertEquals(xml"<foo", List((4, "'>' expected but end of source found")))
  }

  @Test def elem6(): Unit = {
    assertEquals(xml"<foo>", List((5, "'</' expected but end of source found")))
  }

  @Test def elem7(): Unit = {
    assertEquals(xml"<foo></bar>", List((0, "closing tag `foo` expected but `bar` found")))
  }

  @Test def elem8(): Unit = {
    assertEquals(xml"<foo><bar></baz></foo>", List((5, "closing tag `bar` expected but `baz` found")))
  }

  @Test def attribute1(): Unit = {
    assertEquals(xml"<foo bar", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute2(): Unit = {
    assertEquals(xml"<foo bar='", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute3(): Unit = {
    assertEquals(xml"""<foo bar='"""", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute4(): Unit = {
    assertEquals(xml"""<foo bar="""", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute5(): Unit = {
    assertEquals(xml"""<foo bar="'""", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute6(): Unit = {
    assertEquals(xml"<foo bar='&'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute7(): Unit = {
    assertEquals(xml"<foo bar='&;'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute8(): Unit = {
    assertEquals(xml"<foo bar='& ;'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute9(): Unit = {
    assertEquals(xml"<foo bar='&123;'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute10(): Unit = {
    assertEquals(xml"<foo bar='&#'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute11(): Unit = {
    assertEquals(xml"<foo bar='&# ;'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute12(): Unit = {
    assertEquals(xml"<foo bar='&#1,23;'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute14(): Unit = {
    assertEquals(xml"<foo bar='&#x'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute15(): Unit = {
    assertEquals(xml"<foo bar='&#x ;'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute16(): Unit = {
    assertEquals(xml"<foo bar='&#x1,23;'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute17(): Unit = {
    assertEquals(xml"<foo bar='&#x123baz;'/>", List((5, "'>' expected but 'b' found")))
  }

  @Test def attribute19(): Unit = {
    assertEquals(xml"<foo bar='alpha' bar='beta'/>", List((17, "attribute `bar` may only be defined once")))
  }

  @Test def pcData1(): Unit = {
    assertEquals(xml"<![CDATA[", List((9, "']]>' expected but end of source found")))
  }

  @Test def pcData2(): Unit = {
    assertEquals(xml"<![CDATA[]]>]]>", List((12, "end of input expected")))
  }

  @Test def pcData3(): Unit = {
    assertEquals(xml"<foo><![CDATA[</foo>", List((5, "'</' expected but '<' found")))
  }

  @Test def procInstr1(): Unit = {
    assertEquals(xml"<? ?>", List((2, "'_' or a letter expected but ' ' found")))
  }

  @Test def procInstr2(): Unit = {
    assertEquals(xml"<?123 ?>", List((2, "'_' or a letter expected but '1' found")))
  }

  @Test def procInstr3(): Unit = {
    assertEquals(xml"<?foo ?>?>", List((8, "end of input expected")))
  }

  @Test def comment1(): Unit = {
    assertEquals(xml"<!--", List((4, "'-->' expected but end of source found")))
  }

  @Test def comment2(): Unit = {
    assertEquals(xml"<!---->-->", List((7, "end of input expected")))
  }

  @Test def comment3(): Unit = {
    assertEquals(xml"<foo><!--</foo>", List((5, "'</' expected but '<' found")))
  }

  @Test def reference1(): Unit = {
    assertEquals(xml"<foo>&#</foo>", List((5, "'</' expected but '&' found")))
  }

  @Test def reference2(): Unit = {
    assertEquals(xml"<foo>&# ;</foo>", List((5, "'</' expected but '&' found")))
  }

  @Test def reference3(): Unit = {
    assertEquals(xml"<foo>&#1,23;</foo>", List((5, "'</' expected but '&' found")))
  }

  @Test def reference4(): Unit = {
    assertEquals(xml"<foo>&#123bar;</foo>", List((5, "'</' expected but '&' found")))
  }

  @Test def reference6(): Unit = {
    assertEquals(xml"<foo>&#x</foo>", List((5, "'</' expected but '&' found")))
  }

  @Test def reference7(): Unit = {
    assertEquals(xml"<foo>&#x ;</foo>", List((5, "'</' expected but '&' found")))
  }

  @Test def reference8(): Unit = {
    assertEquals(xml"<foo>&#x123bar;</foo>", List((5, "'</' expected but '&' found")))
  }

  @Test def placeholder1(): Unit = {
    assertEquals(xml"<foo>${}", List((8, "'</' expected but end of source found")))
  }

  @Test def placeholder2(): Unit = {
    assertEquals(xml"<foo>${1 + 2}", List((13, "'</' expected but end of source found")))
  }

  @Test def placeholder3(): Unit = {
    assertEquals(xml"<foo>${1 + 2}<bar/>", List((19, "'</' expected but end of source found")))
  }

  @Test def placeholder4(): Unit = {
    assertEquals(xml"<foo>${1 + 2}${true}", List((20, "'</' expected but end of source found")))
  }

  @Test def placeholder5(): Unit = {
    assertEquals(xml"<foo>${1 + 2} ${true}", List((21, "'</' expected but end of source found")))
  }

  @Test def placeholder6(): Unit = {
    assertEquals(xml"<foo>${1 + 2}${true}<bar/>", List((26, "'</' expected but end of source found")))
  }

  @Test def placeholder7(): Unit = {
    assertEquals(xml"<foo>${1 + 2}<bar/>${true}", List((26, "'</' expected but end of source found")))
  }

  @Test def placeholder8(): Unit = {
    assertEquals(xml"<foo>${1 + 2}<bar/>${true}<!--baz-->", List((36, "'</' expected but end of source found")))
  }

  @Test def placeholder9(): Unit = {
    assertEquals(xml"""<foo bar=${"baz"}>""", List((18, "'</' expected but end of source found")))
  }

  @Test def placeholder10(): Unit = {
    assertEquals(xml"""<foo bar=${"baz"}> """, List((19, "'</' expected but end of source found")))
  }

  @Test def placeholder11(): Unit = {
    assertEquals(xml"""<foo bar=${"baz"}><!--qux-->""", List((28, "'</' expected but end of source found")))
  }

  @Test def placeholder12(): Unit = {
    assertEquals(xml"""<foo bar=${"baz"}>${1 + 2}""", List((26, "'</' expected but end of source found")))
  }

  @Test def placeholder13(): Unit = {
    assertEquals(xml"""<foo bar=${"baz"}>${1 + 2}<!--qux-->""", List((36, "'</' expected but end of source found")))
  }

  @Test def placeholder14(): Unit = {
    assertEquals(xml"""<foo bar=${"baz"}>${1 + 2}${true}<!--qux-->""", List((43, "'</' expected but end of source found")))
  }

  @Test def placeholder15(): Unit = {
    assertEquals(xml"""<foo bar=${"baz"}>${1 + 2} ${true}<!--qux-->""", List((44, "'</' expected but end of source found")))
  }
}
