package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

class ReporterTest {

  implicit object StringContextOps {
    inline def (ctx: => StringContext) xml (args: => Any*) <: Any =
      ${ dotty.xml.interpolator.internal.Macro.implErrors('ctx, 'args) }
  }

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

  @Test def attribute1(): Unit = {
    assertEquals(xml"<foo bar", List((8, "'=' expected but end of source found")))
  }

  @Test def attribute2(): Unit = {
    assertEquals(xml"<foo bar='", List((10, "''' expected but end of source found")))
  }

  @Test def attribute3(): Unit = {
    assertEquals(xml"""<foo bar='"""", List((11, "''' expected but end of source found")))
  }

  @Test def attribute4(): Unit = {
    assertEquals(xml"""<foo bar="""", List((10, "'\"' expected but end of source found")))
  }

  @Test def attribute5(): Unit = {
    assertEquals(xml"""<foo bar="'""", List((11, "'\"' expected but end of source found")))
  }

  @Test def attribute6(): Unit = {
    assertEquals(xml"<foo bar='&'/>", List((11, "'_' or a letter expected but ''' found")))
  }

  @Test def attribute7(): Unit = {
    assertEquals(xml"<foo bar='&;'/>", List((11, "'_' or a letter expected but ';' found")))
  }

  @Test def attribute8(): Unit = {
    assertEquals(xml"<foo bar='& ;'/>", List((11, "'_' or a letter expected but ' ' found")))
  }

  @Test def attribute9(): Unit = {
    assertEquals(xml"<foo bar='&123;'/>", List((11, "'_' or a letter expected but '1' found")))
  }

  @Test def attribute10(): Unit = {
    assertEquals(xml"<foo bar='&#'/>", List((12, "';' expected but ''' found")))
  }

  @Test def attribute11(): Unit = {
    assertEquals(xml"<foo bar='&# ;'/>", List((12, "';' expected but ' ' found")))
  }

  @Test def attribute12(): Unit = {
    assertEquals(xml"<foo bar='&#1,23;'/>", List((13, "';' expected but ',' found")))
  }

  @Test def attribute13(): Unit = {
    // assertEquals(xml"<foo bar='&#1234567890123456789;'/>", List((0, "")))
    fail
  }

  @Test def attribute14(): Unit = {
    assertEquals(xml"<foo bar='&#x'/>", List((13, "';' expected but ''' found")))
  }

  @Test def attribute15(): Unit = {
    assertEquals(xml"<foo bar='&#x ;'/>", List((13, "';' expected but ' ' found")))
  }

  @Test def attribute16(): Unit = {
    assertEquals(xml"<foo bar='&#x1,23;'/>", List((14, "';' expected but ',' found")))
  }

  @Test def attribute17(): Unit = {
    assertEquals(xml"<foo bar='&#x123baz;'/>", List((18, "';' expected but 'z' found")))
  }

  @Test def attribute18(): Unit = {
    // assertEquals(xml"<foo bar='&#x123ABCDEF;'/>", List((0, "")))
    fail
  }

  @Test def attribute19(): Unit = {
    assertEquals(xml"<foo bar='' bar=''/>", List((12, "attribute bar may only be defined once")))
  }

  @Test def pcData1(): Unit = {
    assertEquals(xml"<![CDATA[", List((9, "']]>' expected but end of source found")))
  }

  @Test def pcData2(): Unit = {
    assertEquals(xml"<![CDATA[]]>]]>", List((12, "unexpected ']' found")))
  }

  @Test def pcData3(): Unit = {
    assertEquals(xml"<foo><![CDATA[</foo>", List((20, "']]>' expected but end of source found")))
  }

  @Test def procInstr1(): Unit = {
    assertEquals(xml"<? ?>", List((2, "'_' or a letter expected but ' ' found")))
  }

  @Test def procInstr2(): Unit = {
    assertEquals(xml"<?123 ?>", List((2, "'_' or a letter expected but '1' found")))
  }

  @Test def procInstr3(): Unit = {
    assertEquals(xml"<?foo ?>?>", List((8, "unexpected '?' found")))
  }

  @Test def comment1(): Unit = {
    assertEquals(xml"<!--", List((4, "'-->' expected but end of source found")))
  }

  @Test def comment2(): Unit = {
    assertEquals(xml"<!---->-->", List((7, "unexpected '-' found")))
  }

  @Test def comment3(): Unit = {
    assertEquals(xml"<foo><!--</foo>", List((15, "'-->' expected but end of source found")))
  }

  @Test def reference1(): Unit = {
    assertEquals(xml"<foo>&#</foo>", List((7, "';' expected but '<' found")))
  }

  @Test def reference2(): Unit = {
    assertEquals(xml"<foo>&# ;</foo>", List((7, "';' expected but ' ' found")))
  }

  @Test def reference3(): Unit = {
    assertEquals(xml"<foo>&#1,23;</foo>", List((8, "';' expected but ',' found")))
  }

  @Test def reference4(): Unit = {
    assertEquals(xml"<foo>&#123bar;</foo>", List((10, "';' expected but 'b' found")))
  }

  @Test def reference5(): Unit = {
    // assertEquals(xml"<foo>&#1234567890123456789;</foo>", List((0, "")))
    fail
  }

  @Test def reference6(): Unit = {
    assertEquals(xml"<foo>&#x</foo>", List((8, "';' expected but '<' found")))
  }

  @Test def reference7(): Unit = {
    assertEquals(xml"<foo>&#x ;</foo>", List((8, "';' expected but ' ' found")))
  }

  @Test def reference8(): Unit = {
    assertEquals(xml"<foo>&#x123bar;</foo>", List((13, "';' expected but 'r' found")))
  }

  @Test def reference9(): Unit = {
    // assertEquals(xml"<foo>&#x123ABCDEF;</foo>", List((0, "")))
    fail
  }
}