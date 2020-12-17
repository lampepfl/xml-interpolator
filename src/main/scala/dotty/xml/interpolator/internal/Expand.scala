package dotty.xml.interpolator
package internal

import scala.language.implicitConversions
import scala.quoted._

import dotty.xml.interpolator.internal.Tree._

object Expand {

  def apply(nodes: Seq[Node])(implicit ctx: XmlContext, q: Quotes): Expr[scala.xml.Node | scala.xml.NodeBuffer] = {
    if (nodes.size == 1) expandNode(nodes.head).asInstanceOf[Expr[scala.xml.Node]]
    else expandNodes(nodes)
  }

  private def expandNode(node: Node)(implicit ctx: XmlContext, q: Quotes): Expr[Any] = {
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

  private def expandNodes(nodes: Seq[Node])(implicit ctx: XmlContext, q: Quotes): Expr[scala.xml.NodeBuffer] = {
    nodes.foldLeft('{ new _root_.scala.xml.NodeBuffer() })((expr, node) => '{ $expr &+ ${expandNode(node)} } )
  }

  private def expandGroup(group: Group)(implicit ctx: XmlContext, q: Quotes): Expr[scala.xml.Group] =
    '{ new _root_.scala.xml.Group(${expandNodes(group.nodes)}) }

  private def expandElem(elem: Elem)(implicit ctx: XmlContext, q: Quotes): Expr[scala.xml.Elem] = {
    val (namespaces, attributes) = elem.attributes.partition(_.isNamespace)
    val prefix = if (elem.prefix.nonEmpty) Expr(elem.prefix) else '{ null: String }
    val label = Expr(elem.label)
    val attributes1 = expandAttributes(attributes)
    val scope = expandNamespaces(namespaces)
    val empty = Expr(elem.end.isEmpty)
    val child = expandNodes(elem.children)(new XmlContext(ctx.args, scope), q)
    if (elem.children.isEmpty)
      '{ new _root_.scala.xml.Elem($prefix, $label, $attributes1, $scope, $empty) }
    else
      '{ new _root_.scala.xml.Elem($prefix, $label, $attributes1, $scope, $empty, _root_.scala.xml.NodeSeq.seqToNodeSeq($child): _*) }
  }

  private def expandAttributes(attributes: Seq[Attribute])(implicit ctx: XmlContext, q: Quotes): Expr[scala.xml.MetaData] = {
    import quotes.reflect._
    attributes.foldRight('{ _root_.scala.xml.Null }: Expr[scala.xml.MetaData])((attribute, rest) => {
      val value = attribute.value match {
          case Seq(v) => expandNode(v)
          case vs     => expandNodes(vs)
      }

      /*
      value match {
        case '{($value: String )} =>
          if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${Expr(attribute.key)}, $value, $rest) }
          else '{ new _root_.scala.xml.PrefixedAttribute(${Expr(attribute.prefix)}, ${Expr(attribute.key)}, $value, $rest) }
        case '{($value: Seq[scala.xml.Node])} =>
          if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${Expr(attribute.key)}, $value, $rest) }
          else '{ new _root_.scala.xml.PrefixedAttribute(${Expr(attribute.prefix)}, ${Expr(attribute.key)}, $value, $rest) }
        case '{($value: Option[Seq[scala.xml.Node]])} =>
          if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${Expr(attribute.key)}, $value, $rest) }
          else '{ new _root_.scala.xml.PrefixedAttribute(${Expr(attribute.prefix)}, ${Expr(attribute.key)}, $value, $rest) }
      }
      */

      val term = value.asTerm
      if (term.tpe <:< TypeRepr.of[String]) {
        val value = term.asExprOf[String]
        if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${Expr(attribute.key)}, $value, $rest) }
        else '{ new _root_.scala.xml.PrefixedAttribute(${Expr(attribute.prefix)}, ${Expr(attribute.key)}, $value, $rest) }
      } else if (term.tpe <:< TypeRepr.of[collection.Seq[scala.xml.Node]]) {
        val value = term.asExprOf[collection.Seq[scala.xml.Node]]
        if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${Expr(attribute.key)}, $value, $rest) }
        else '{ new _root_.scala.xml.PrefixedAttribute(${Expr(attribute.prefix)}, ${Expr(attribute.key)}, $value, $rest) }
      } else {
        val value = term.asExprOf[Option[collection.Seq[scala.xml.Node]]]
        if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${Expr(attribute.key)}, $value, $rest) }
        else '{ new _root_.scala.xml.PrefixedAttribute(${Expr(attribute.prefix)}, ${Expr(attribute.key)}, $value, $rest) }
      }
    })
  }

  private def expandNamespaces(namespaces: Seq[Attribute])(implicit ctx: XmlContext, q: Quotes): Expr[scala.xml.NamespaceBinding] = {
    import quotes.reflect._
    namespaces.foldLeft(ctx.scope)((rest, namespace) => {
      val prefix = if (namespace.prefix.nonEmpty) Expr(namespace.key) else '{ null: String }
      val uri = (namespace.value.head: @unchecked) match {
        case Text(text) => Expr(text)
        case Placeholder(id) =>
          val call = '{ ${ctx.args(id)}(using _root_.scala.xml.TopScope) }
          Expr.betaReduce(call).asExprOf[String]
      }
      '{ new _root_.scala.xml.NamespaceBinding($prefix, $uri, $rest) }
    })
  }

  private def expandText(text: Text)(using Quotes): Expr[scala.xml.Text] =
    '{ new _root_.scala.xml.Text(${Expr(text.text)}) }

  private def expandComment(comment: Comment)(using Quotes): Expr[scala.xml.Comment] =
    '{ new _root_.scala.xml.Comment(${Expr(comment.text)}) }

  private def expandPlaceholder(placeholder: Placeholder)(implicit ctx: XmlContext, q: Quotes): Expr[Any] = {
    val arg = ctx.args(placeholder.id)
    val scope = ctx.scope
    Expr.betaReduce('{ $arg(using $scope) })
  }

  private def expandPCData(pcdata: PCData)(using Quotes): Expr[scala.xml.PCData] =
    '{ new _root_.scala.xml.PCData(${Expr(pcdata.data)}) }

  private def expandProcInstr(instr: ProcInstr)(using Quotes): Expr[scala.xml.ProcInstr] =
    '{ new _root_.scala.xml.ProcInstr(${Expr(instr.target)}, ${Expr(instr.proctext)}) }

  private def expandEntityRef(ref: EntityRef)(using Quotes): Expr[scala.xml.EntityRef] =
    '{ new _root_.scala.xml.EntityRef(${Expr(ref.name)}) }

  private def expandUnparsed(unparsed: Unparsed)(using Quotes): Expr[scala.xml.Unparsed] =
    '{ new _root_.scala.xml.Unparsed(${Expr(unparsed.data)}) }
}
