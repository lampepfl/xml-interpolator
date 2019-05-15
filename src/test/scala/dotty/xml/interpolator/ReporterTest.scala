package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

class ReporterTest {

  implicit object StringContextOps {
    inline def (ctx: => StringContext) xml (args: => Any*) <: Any =
      ${ dotty.xml.interpolator.internal.Macro.implErrors('ctx, 'args) }
  }

  @Test def test1(): Unit = {
    assertEquals(xml"", List(0, 0, ""))
  }

  @Test def test2(): Unit = {
    assertEquals(xml" ", List(0, 0, ""))
  }

  @Test def test3(): Unit = {
    assertEquals(xml"123", List(0, 0, ""))
  }

  @Test def test4(): Unit = {
    assertEquals(xml"foo", List(0, 0, ""))
  }

  @Test def test5(): Unit = {
    assertEquals(xml"<>", List(0, 0, ""))
  }

  @Test def test6(): Unit = {
    assertEquals(xml"<123/>", List(0, 0, ""))
  }

  @Test def test7(): Unit = {
    assertEquals(xml"<<foo/>", List(0, 0, ""))
  }

  @Test def test8(): Unit = {
    assertEquals(xml"<foo/>/>", List(0, 0, ""))
  }

  @Test def test9(): Unit = {
    assertEquals(xml"<123foo/>", List(0, 0, ""))
  }

  @Test def test10(): Unit = {
    assertEquals(xml"< foo/>", List(0, 0, ""))
  }

  @Test def test11(): Unit = {
    assertEquals(xml"<foo?/>", List(0, 0, ""))
  }

  @Test def test12(): Unit = {
    assertEquals(xml"<foo>", List(0, 0, ""))
  }

  @Test def test13(): Unit = {
    assertEquals(xml"</foo>", List(0, 0, ""))
  }

  @Test def test14(): Unit = {
    assertEquals(xml"<foo></bar>", List(0, 0, ""))
    // todo
  }

  @Test def test15(): Unit = {
    assertEquals(xml"<foo bar", List(0, 0, ""))
  }

  @Test def test16(): Unit = {
    assertEquals(xml"<foo bar=", List(0, 0, ""))
  }

  @Test def test17(): Unit = {
    assertEquals(xml"<foo bar='", List(0, 0, ""))
  }

  @Test def test18(): Unit = {
    assertEquals(xml"<foo bar='", List(0, 0, ""))
  }

  @Test def test19(): Unit = {
    assertEquals(xml"""<foo bar="'""", List(0, 0, ""))
  }

  @Test def test20(): Unit = {
    assertEquals(xml"<foo 123=''/>", List(0, 0, ""))
  }

  @Test def test21(): Unit = {
    assertEquals(xml"<foo 123bar=''/>", List(0, 0, ""))
  }

  @Test def test22(): Unit = {
    assertEquals(xml"<foo bar?=''/>", List(0, 0, ""))
  }

  @Test def test23(): Unit = {
    assertEquals(xml"<foo bar='&'/>", List(0, 0, ""))
  }

  @Test def test24(): Unit = {
    assertEquals(xml"<foo bar='&;'/>", List(0, 0, ""))
  }

  @Test def test25(): Unit = {
    assertEquals(xml"<foo bar='& ;'/>", List(0, 0, ""))
  }

  @Test def test26(): Unit = {
    assertEquals(xml"<foo bar='&123;'/>", List(0, 0, ""))
  }

  @Test def test27(): Unit = {
    assertEquals(xml"<foo bar='&123baz;'/>", List(0, 0, ""))
  }

  @Test def test28(): Unit = {
    assertEquals(xml"<foo bar='&#'/>", List(0, 0, ""))
  }

  @Test def test29(): Unit = {
    assertEquals(xml"<foo bar='&# ;'/>", List(0, 0, ""))
  }

  @Test def test30(): Unit = {
    assertEquals(xml"<foo bar='&#1,23;'/>", List(0, 0, ""))
  }

  @Test def test31(): Unit = {
    assertEquals(xml"<foo bar='&#1.23;'/>", List(0, 0, ""))
  }

  @Test def test32(): Unit = {
    //xml"<foo bar='&#1234567890123456789;'/>"
    //todo
    fail
  }

  @Test def test33(): Unit = {
    assertEquals(xml"<foo bar='&#x'/>", List(0, 0, ""))
  }

  @Test def test34(): Unit = {
    assertEquals(xml"<foo bar='&#x ;'/>", List(0, 0, ""))
  }

  @Test def test35(): Unit = {
    assertEquals(xml"<foo bar='&#x1,23;'/>", List(0, 0, ""))
  }

  @Test def test36(): Unit = {
    assertEquals(xml"<foo bar='&#x123baz;'/>", List(0, 0, ""))
  }

  @Test def test37(): Unit = {
    //xml"<foo bar='&#x123ABCDEF;'/>"
    //todo
    fail
  }

  @Test def test38(): Unit = {
    assertEquals(xml"<foo bar='' bar=''/>", List(0, 0, ""))
  }

  @Test def test39(): Unit = {
    //xml"<foo bar=${1 + 2}/>"
    //todo
    fail
  }

  @Test def test40(): Unit = {
    //xml"<foo bar=${1.5}/>"
    //todo
    fail
  }

  @Test def test41(): Unit = {
    //xml"<foo bar=${true}/>"
    //todo
    fail
  }

  @Test def test42(): Unit = {
    //xml"<foo bar=${1.toChar}/>"s
    //todo
    fail
  }

  @Test def test43(): Unit = {
    //xml"<foo bar=${List[Int]()}/>"
    //todo
    fail
  }

  @Test def test44(): Unit = {
    //xml"<foo bar=${Some(1)}/>"
    //todo
    fail
  }

  @Test def test45(): Unit = {
    assertEquals(xml"<![CDATA[", List(0, 0, ""))
  }

  @Test def test46(): Unit = {
    assertEquals(xml"<![CDATA[]]>]]>", List(0, 0, ""))
  }

  @Test def test47(): Unit = {
    assertEquals(xml"<![CDATA[]]>]]>", List(0, 0, ""))
  }

  @Test def test48(): Unit = {
    assertEquals(xml"<?", List(0, 0, ""))
  }

  @Test def test49(): Unit = {
    assertEquals(xml"<?123 ?>", List(0, 0, ""))
  }

  @Test def test50(): Unit = {
    assertEquals(xml"<?123foo ?>", List(0, 0, ""))
  }

  @Test def test51(): Unit = {
    assertEquals(xml"<?foo ?>?>", List(0, 0, ""))
  }

  @Test def test52(): Unit = {
    assertEquals(xml"<!--", List(0, 0, ""))
  }

  @Test def test53(): Unit = {
    assertEquals(xml"<!---->-->", List(0, 0, ""))
  }

  @Test def test54(): Unit = {
    assertEquals(xml"<foo>&#</foo>", List(0, 0, ""))
  }

  @Test def test55(): Unit = {
    assertEquals(xml"<foo>&# ;</foo>", List(0, 0, ""))
  }

  @Test def test56(): Unit = {
    assertEquals(xml"<foo>&#1,23;</foo>", List(0, 0, ""))
  }

  @Test def test57(): Unit = {
    assertEquals(xml"<foo>&#123bar;</foo>", List(0, 0, ""))
  }

  @Test def test58(): Unit = {
    //xml"<foo>&#1234567890123456789;</foo>"
    //todo
    fail
  }

  @Test def test59(): Unit = {
    assertEquals(xml"<foo>&#x</foo>", List(0, 0, ""))
  }

  @Test def test60(): Unit = {
    assertEquals(xml"<foo>&#x ;</foo>", List(0, 0, ""))
  }

  @Test def test61(): Unit = {
    assertEquals(xml"<foo>&#x123bar;</foo>", List(0, 0, ""))
  }

  @Test def test62(): Unit = {
    //xml"<foo>&#x123ABCDEF;</foo>"
    //todo
    fail
  }

  @Test def test63(): Unit = {
    assertEquals(xml"<foo><![CDATA[</foo>", List(0, 0, ""))
  }

  @Test def test64(): Unit = {
    assertEquals(xml"<foo><?</foo>", List(0, 0, ""))
  }

  @Test def test65(): Unit = {
    assertEquals(xml"<foo><?123 ?></foo>", List(0, 0, ""))
  }

  @Test def test66(): Unit = {
    assertEquals(xml"<foo><?123bar ?></foo>", List(0, 0, ""))
  }

  @Test def test67(): Unit = {
    assertEquals(xml"<foo><!--</foo>", List(0, 0, ""))
  }
  
  @Test def test68(): Unit = {
    assertEquals(xml"<foo>${}&#</foo>", List(0, 0, ""))
  }

  @Test def test69(): Unit = {
    assertEquals(xml"<foo>${}&# ;</foo>", List(0, 0, ""))
  }

  @Test def test70(): Unit = {
    assertEquals(xml"<foo>${}'&#1,23;</foo>", List(0, 0, ""))
  }

  @Test def test71(): Unit = {
    assertEquals(xml"<foo>${}&#123bar;</foo>", List(0, 0, ""))
  }

  @Test def test72(): Unit = {
    //xml"<foo>${}&#1234567890123456789;</foo>"
    //todo
    fail
  }

  @Test def test73(): Unit = {
    assertEquals(xml"<foo>${}&#x</foo>", List(0, 0, ""))
  }

  @Test def test74(): Unit = {
    assertEquals(xml"<foo>${}&#x ;</foo>", List(0, 0, ""))
  }

  @Test def test75(): Unit = {
    assertEquals(xml"<foo>${}&#x1,23;</foo>", List(0, 0, ""))
  }

  @Test def test76(): Unit = {
    assertEquals(xml"<foo>${}&#x123bar;</foo>", List(0, 0, ""))
  }

  @Test def test77(): Unit = {
    //xml"<foo>${}&#x123ABCDEF;</foo>"
    //todo
    fail
  }

  @Test def test78(): Unit = {
    assertEquals(xml"<foo>${}<![CDATA[</foo>", List(0, 0, ""))
  }

  @Test def test79(): Unit = {
    assertEquals(xml"<foo>${}<?</foo>", List(0, 0, ""))
  }

  @Test def test80(): Unit = {
    assertEquals(xml"<foo>${}<?123 ?></foo>", List(0, 0, ""))
  }

  @Test def test81(): Unit = {
    assertEquals(xml"<foo>${}<?123bar ?></foo>", List(0, 0, ""))
  }

  @Test def test82(): Unit = {
    assertEquals(xml"<foo>${}<!--</foo>", List(0, 0, ""))
  }

  @Test def test83(): Unit = {
    assertEquals(xml"<foo>${1 + 2}&#</foo>", List(0, 0, ""))
  }

  @Test def test84(): Unit = {
    assertEquals(xml"<foo>${1 + 2}'&#1,23;</foo>", List(0, 0, ""))
  }

  @Test def test85(): Unit = {
    assertEquals(xml"<foo>${1 + 2}&#123bar;</foo>", List(0, 0, ""))
  }

  @Test def test86(): Unit = {
    //xml"<foo>${1 + 2}&#1234567890123456789;</foo>"
    //todo
    fail
  }

  @Test def test87(): Unit = {
    assertEquals(xml"<foo>${1 + 2}&#x</foo>", List(0, 0, ""))
  }

  @Test def test88(): Unit = {
    assertEquals(xml"<foo>${1 + 2}&#x ;</foo>", List(0, 0, ""))
  }

  @Test def test89(): Unit = {
    assertEquals(xml"<foo>${1 + 2}&#x1,23;</foo>", List(0, 0, ""))
  }

  @Test def test90(): Unit = {
    assertEquals(xml"<foo>${1 + 2}&#x123bar;</foo>", List(0, 0, ""))
  }

  @Test def test91(): Unit = {
    //xml"<foo>${1 + 2}&#x123ABCDEF;</foo>"
    //todo
    fail
  }

  @Test def test92(): Unit = {
    assertEquals(xml"<foo>${1 + 2}<![CDATA[</foo>", List(0, 0, ""))
  }

  @Test def test93(): Unit = {
    assertEquals(xml"<foo>${1 + 2}<?</foo>", List(0, 0, ""))
  }

  @Test def test94(): Unit = {
    assertEquals(xml"<foo>${1 + 2}<?123 ?></foo>", List(0, 0, ""))
  }

  @Test def test95(): Unit = {
    assertEquals(xml"<foo>${1 + 2}<?123bar ?></foo>", List(0, 0, ""))
  }

  @Test def test96(): Unit = {
    assertEquals(xml"<foo>${1 + 2}<!--</foo>", List(0, 0, ""))
  }
}