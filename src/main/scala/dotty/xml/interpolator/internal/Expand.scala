package dotty.xml.interpolator
package internal

import scala.language.implicitConversions
import scala.quoted._

import dotty.xml.interpolator.internal.Tree._

object Expand {

  def apply(nodes: Seq[Node])(implicit ctx: XmlContext, qctx: QuoteContext): Expr[scala.xml.Node | scala.xml.NodeBuffer] = {
    if (nodes.size == 1) expandNode(nodes.head).asInstanceOf[Expr[scala.xml.Node]]
    else expandNodes(nodes)
  }

  private def expandNode(node: Node)(implicit ctx: XmlContext, qctx: QuoteContext): Expr[Any] = {
    node match {
      case group: Group             => expandGroup(group)
      case elem: Elem               => expandElem(elem)
      case text: Text               => expandText(text)
      case comment: Comment         => expandComment(comment)
      case placeholder: Placeholder => expandPlaceholder(placeholder)
      case pcData: PCData           => expandPCData(pcData)
      case procInstr: ProcInstr     => expandProcInstr(procInstr)
      case entityRef: EntityRef     => expandEntityRef(entityRef)
      case unparsed: Unparsed       => expandUnparsed(unparsed)
    }
  }

  private def expandNodes(nodes: Seq[Node])(implicit ctx: XmlContext, qctx: QuoteContext): Expr[scala.xml.NodeBuffer] = {
    nodes.foldLeft('{ new _root_.scala.xml.NodeBuffer() })((expr, node) => '{ $expr &+ ${expandNode(node)} } )
  }

  private def expandGroup(group: Group)(implicit ctx: XmlContext, qctx: QuoteContext): Expr[scala.xml.Group] =
    '{ new _root_.scala.xml.Group(${expandNodes(group.nodes)}) }

  private def expandElem(elem: Elem)(implicit ctx: XmlContext, qctx: QuoteContext): Expr[scala.xml.Elem] = {
    val (namespaces, attributes) = elem.attributes.partition(_.isNamespace)
    val prefix = if (elem.prefix.nonEmpty) elem.prefix.toExpr else '{ null: String }
    val label = elem.label.toExpr
    val attributes1 = expandAttributes(attributes)
    val scope = expandNamespaces(namespaces)
    val empty = elem.end.isEmpty.toExpr
    val child = expandNodes(elem.children)(new XmlContext(ctx.args, scope), qctx)
    if (elem.children.isEmpty)
      '{ new _root_.scala.xml.Elem($prefix, $label, $attributes1, $scope, $empty) }
    else
      '{ new _root_.scala.xml.Elem($prefix, $label, $attributes1, $scope, $empty, $child: _*) }
  }

  private def expandAttributes(attributes: Seq[Attribute])(implicit ctx: XmlContext, qctx: QuoteContext): Expr[scala.xml.MetaData] = {
    import qctx.tasty._
    attributes.foldRight('{ _root_.scala.xml.Null }: Expr[scala.xml.MetaData])((attribute, rest) => {
      val value = attribute.value match {
          case Seq(v) => expandNode(v)
          case vs     => expandNodes(vs)
      }

      /*
      value match {
        case '{($value: String )} =>
          if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${attribute.key.toExpr}, $value, $rest) }
          else '{ new _root_.scala.xml.PrefixedAttribute(${attribute.prefix.toExpr}, ${attribute.key.toExpr}, $value, $rest) }
        case '{($value: Seq[scala.xml.Node])} =>
          if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${attribute.key.toExpr}, $value, $rest) }
          else '{ new _root_.scala.xml.PrefixedAttribute(${attribute.prefix.toExpr}, ${attribute.key.toExpr}, $value, $rest) }
        case '{($value: Option[Seq[scala.xml.Node]])} =>
          if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${attribute.key.toExpr}, $value, $rest) }
          else '{ new _root_.scala.xml.PrefixedAttribute(${attribute.prefix.toExpr}, ${attribute.key.toExpr}, $value, $rest) }
      }
      */

      val term = value.unseal
      if (term.tpe <:< '[String].unseal.tpe) {
        val value = term.seal.cast[String]
        if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${attribute.key.toExpr}, $value, $rest) }
        else '{ new _root_.scala.xml.PrefixedAttribute(${attribute.prefix.toExpr}, ${attribute.key.toExpr}, $value, $rest) }
      } else if (term.tpe <:< '[Seq[scala.xml.Node]].unseal.tpe) {
        val value = term.seal.cast[Seq[scala.xml.Node]]
        if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${attribute.key.toExpr}, $value, $rest) }
        else '{ new _root_.scala.xml.PrefixedAttribute(${attribute.prefix.toExpr}, ${attribute.key.toExpr}, $value, $rest) }
      } else {
        val value = term.seal.cast[Option[Seq[scala.xml.Node]]]
        if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${attribute.key.toExpr}, $value, $rest) }
        else '{ new _root_.scala.xml.PrefixedAttribute(${attribute.prefix.toExpr}, ${attribute.key.toExpr}, $value, $rest) }
      }
    })
  }

  private def expandNamespaces(namespaces: Seq[Attribute])(implicit ctx: XmlContext, qctx: QuoteContext): Expr[scala.xml.NamespaceBinding] = {
    import qctx.tasty._
    namespaces.foldLeft(ctx.scope)((rest, namespace) => {
      val prefix = if (namespace.prefix.nonEmpty) namespace.key.toExpr else '{ null: String }
      val uri = (namespace.value.head: @unchecked) match {
        case Text(text) => text.toExpr
        case Placeholder(id) => ctx.args(id).apply('{ _root_.scala.xml.TopScope }).asInstanceOf[Expr[String]]
      }
      '{ new _root_.scala.xml.NamespaceBinding($prefix, $uri, $rest) }
    })
  }

  private def expandText(text: Text): Expr[scala.xml.Text] =
    '{ new _root_.scala.xml.Text(${text.text.toExpr}) }

  private def expandComment(comment: Comment): Expr[scala.xml.Comment] =
    '{ new _root_.scala.xml.Comment(${comment.text.toExpr}) }

  private def expandPlaceholder(placeholder: Placeholder)(implicit ctx: XmlContext): Expr[Any] = {
    ctx.args(placeholder.id).apply(ctx.scope)
  }

  private def expandPCData(pcdata: PCData): Expr[scala.xml.PCData] =
    '{ new _root_.scala.xml.PCData(${pcdata.data.toExpr}) }

  private def expandProcInstr(instr: ProcInstr): Expr[scala.xml.ProcInstr] =
    '{ new _root_.scala.xml.ProcInstr(${instr.target.toExpr}, ${instr.proctext.toExpr}) }

  private def expandEntityRef(ref: EntityRef): Expr[scala.xml.EntityRef] =
    '{ new _root_.scala.xml.EntityRef(${ref.name.toExpr}) }

  private def expandUnparsed(unparsed: Unparsed): Expr[scala.xml.Unparsed] =
    '{ new _root_.scala.xml.Unparsed(${unparsed.data.toExpr}) }
}