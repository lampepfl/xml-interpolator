package dotty.xml.interpolator

import org.junit.Test
import org.junit.Assert._

import scala.quoted._
import scala.quoted.Toolbox.Default._

import Tree._

class LiftTest {

  @Test def liftNode(): Unit = {
    val xml = Seq(Elem("foo", Seq(Attribute("xmlns:bar", Seq(Text("baz"))), Attribute("baz", Seq(Text("qux")))), empty = false, Seq(Elem("bar:qun", Seq(Attribute("quz", Seq(Text("qur")))), empty = true, Seq()))))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.Elem((null: scala.Predef.String), "foo", new _root_.scala.xml.UnprefixedAttribute("baz", new _root_.scala.xml.Text("qux"), _root_.scala.xml.Null), new _root_.scala.xml.NamespaceBinding("bar", "baz", _root_.scala.xml.TopScope), false, new _root_.scala.xml.NodeBuffer().&+(new _root_.scala.xml.Elem("bar", "qun", new _root_.scala.xml.UnprefixedAttribute("quz", new _root_.scala.xml.Text("qur"), _root_.scala.xml.Null), new _root_.scala.xml.NamespaceBinding("bar", "baz", _root_.scala.xml.TopScope), true)): _*)""",
      exp.show
    )
  }

  @Test def liftNodes(): Unit = {
    val xml = Seq(Elem("foo", Seq(), empty = true, Seq()), Elem("bar", Seq(), empty = true, Seq()))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.NodeBuffer().&+(new _root_.scala.xml.Elem((null: scala.Predef.String), "bar", _root_.scala.xml.Null, _root_.scala.xml.TopScope, true)).&+(new _root_.scala.xml.Elem((null: scala.Predef.String), "foo", _root_.scala.xml.Null, _root_.scala.xml.TopScope, true))""",
      exp.show
    )
  }

  @Test def liftEmptyElem(): Unit = {
    val xml = Seq(Elem("foo", Seq(), empty = true, Seq()))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.Elem((null: scala.Predef.String), "foo", _root_.scala.xml.Null, _root_.scala.xml.TopScope, true)""",
      exp.show
    )
  }

  @Test def liftElem(): Unit = {
    val xml = Seq(Elem("foo", Seq(), empty = false, Seq(Text("bar"))))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.Elem((null: scala.Predef.String), "foo", _root_.scala.xml.Null, _root_.scala.xml.TopScope, false, new _root_.scala.xml.NodeBuffer().&+(new _root_.scala.xml.Text("bar")): _*)""",
      exp.show
    )
  }

  @Test def liftAttributes(): Unit = {
    val xml = Seq(Elem("foo", Seq(Attribute("bar", Seq(Text("baz"))), Attribute("baz:qux", Seq(Text("qun"))), Attribute("qux", Seq(Placeholder(0)))), empty = true, Seq()))
    val exp = Lift(xml, List(1.toExpr))
    assertEquals(
      """new _root_.scala.xml.Elem((null: scala.Predef.String), "foo", new _root_.scala.xml.UnprefixedAttribute("bar", new _root_.scala.xml.Text("baz"), new _root_.scala.xml.PrefixedAttribute("baz", "qux", new _root_.scala.xml.Text("qun"), new _root_.scala.xml.UnprefixedAttribute("qux", 1, _root_.scala.xml.Null))), _root_.scala.xml.TopScope, true)""",
      exp.show
    )
  }

  @Test def liftNamespaces(): Unit = {
    val xml = Seq(Elem("foo", Seq(Attribute("xmlns:bar", Seq(Text("baz"))), Attribute("baz", Seq(Text("qux")))), empty = true, Seq()))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.Elem((null: scala.Predef.String), "foo", new _root_.scala.xml.UnprefixedAttribute("baz", new _root_.scala.xml.Text("qux"), _root_.scala.xml.Null), new _root_.scala.xml.NamespaceBinding("bar", "baz", _root_.scala.xml.TopScope), true)""",
      exp.show
    )
  }

  @Test def liftPlaceholder(): Unit = {
    val xml = Seq(Elem("foo", Seq(), empty = true, Seq(Placeholder(0))))
    val exp = Lift(xml, List(1.toExpr))
    assertEquals(
      """new _root_.scala.xml.Elem((null: scala.Predef.String), "foo", _root_.scala.xml.Null, _root_.scala.xml.TopScope, true, new _root_.scala.xml.NodeBuffer().&+(1): _*)""",
      exp.show
    )
  }

  @Test def liftText(): Unit = {
    val xml = Seq(Text("foo"))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.Text("foo")""",
      exp.show
    )
  }

  @Test def liftComment(): Unit = {
    val xml = Seq(Comment("foo"))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.Comment("foo")""",
      exp.show
    )
  }

  @Test def liftPCData(): Unit = {
    val xml = Seq(PCData("foo"))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.PCData("foo")""",
      exp.show
    )
  }

  @Test def liftProcInstr(): Unit = {
    val xml = Seq(ProcInstr("foo", "bar"))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.ProcInstr("foo", "bar")""",
      exp.show
    )
  }

  @Test def liftEntityRef(): Unit = {
    val xml = Seq(EntityRef("foo"))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.EntityRef("foo")""",
      exp.show
    )
  }

  @Test def liftUnparsed(): Unit = {
    val xml = Seq(Unparsed("foo"))
    val exp = Lift(xml, Nil)
    assertEquals(
      """new _root_.scala.xml.Unparsed("foo")""",
      exp.show
    )
  }
}