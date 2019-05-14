package dotty.xml.interpolator.internal

import scala.language.implicitConversions
import scala.quoted._
import scala.quoted.Exprs.LiftedExpr
import scala.tasty.Reflection

import dotty.xml.interpolator.internal.Tree._

object Generate {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(this.getClass.getClassLoader)

  def apply(nodes: Seq[Node]) given Seq[Expr[Any]] given Reflection: Expr[xml.Node | xml.NodeBuffer] = {
    implicit val scope = '{ _root_.scala.xml.TopScope }
    if (nodes.size == 1) liftNode(nodes.head).asInstanceOf[Expr[scala.xml.Node]]
    else liftNodes(nodes)
  }

  private def liftNode(node: Node) given Seq[Expr[Any]] given Expr[scala.xml.NamespaceBinding] given Reflection = {
    node match {
      case group: Group             => liftGroup(group)
      case elem: Elem               => liftElem(elem)
      case text: Text               => liftText(text)
      case comment: Comment         => liftComment(comment)
      case placeholder: Placeholder => liftPlaceholder(placeholder)
      case pcData: PCData           => liftPCData(pcData)
      case procInstr: ProcInstr     => liftProcInstr(procInstr)
      case entityRef: EntityRef     => liftEntityRef(entityRef)
      case unparsed: Unparsed       => liftUnparsed(unparsed)
    }
  }

  private def liftNodes(nodes: Seq[Node]) given Seq[Expr[Any]] given Expr[scala.xml.NamespaceBinding] given Reflection: Expr[scala.xml.NodeBuffer] = {
    nodes.foldLeft('{ new _root_.scala.xml.NodeBuffer() })((expr, node) => '{ $expr &+ ${liftNode(node)} } )
  }

  private def liftGroup(group: Group) given Seq[Expr[Any]] given Expr[scala.xml.NamespaceBinding] given Reflection = '{
    new _root_.scala.xml.Group(${liftNodes(group.nodes)})
  }

  private def liftElem(elem: Elem) given Seq[Expr[Any]] given Expr[scala.xml.NamespaceBinding] given Reflection= {
    val (namespaces, attributes) = elem.attributes.partition(_.isNamespace)
    val prefix = if (elem.prefix.nonEmpty) elem.prefix.toExpr else '{ null: String }
    val label = elem.label.toExpr
    val attributes1 = liftAttributes(attributes)
    val scope = liftNamespaces(namespaces)
    val empty = elem.empty.toExpr
    val child = liftNodes(elem.children)
    if (elem.children.isEmpty)
      '{ new _root_.scala.xml.Elem($prefix, $label, $attributes1, $scope, $empty) }
    else
      '{ new _root_.scala.xml.Elem($prefix, $label, $attributes1, $scope, $empty, $child: _*) }
  }

  private def liftAttributes(attributes: Seq[Attribute]) given Seq[Expr[Any]] given Expr[scala.xml.NamespaceBinding] given (reflect: Reflection): Expr[scala.xml.MetaData] = {
    import reflect._
    attributes.foldRight('{ _root_.scala.xml.Null }: Expr[scala.xml.MetaData])((attribute, rest) => {
      val value = attribute.value match {
          case Seq(v) => liftNode(v)
          case vs     => liftNodes(vs)
      }
      val term = value.unseal
      if (term.tpe <:< '[String].unseal.tpe) {
        val v = term.seal.cast[String]
        if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${attribute.key.toExpr}, $v, $rest) }
        else '{ new _root_.scala.xml.PrefixedAttribute(${attribute.prefix.toExpr}, ${attribute.key.toExpr}, $v, $rest) }
      } else if (term.tpe <:< '[Seq[scala.xml.Node]].unseal.tpe) {
        val v = term.seal.cast[Seq[scala.xml.Node]]
        if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${attribute.key.toExpr}, $v, $rest) }
        else '{ new _root_.scala.xml.PrefixedAttribute(${attribute.prefix.toExpr}, ${attribute.key.toExpr}, $v, $rest) }
      } else {
        val v = term.seal.cast[Option[Seq[scala.xml.Node]]]
        if (attribute.prefix.isEmpty) '{ new _root_.scala.xml.UnprefixedAttribute(${attribute.key.toExpr}, $v, $rest) }
        else '{ new _root_.scala.xml.PrefixedAttribute(${attribute.prefix.toExpr}, ${attribute.key.toExpr}, $v, $rest) }
      }
    })
  }

  private def liftNamespaces(namespaces: Seq[Attribute]) given (args: Seq[Expr[Any]]) given (outer: Expr[scala.xml.NamespaceBinding]) given (reflect: Reflection): Expr[scala.xml.NamespaceBinding] = {
    import reflect._
    namespaces.foldLeft(outer)((rest, namespace) => {
      val prefix = if (namespace.prefix.nonEmpty) namespace.key.toExpr else '{ null: String }
      val uri = (namespace.value.head: @unchecked) match {
        case Text(text) => text.toExpr
        case Placeholder(id) => args(id).asInstanceOf[Expr[String]]
      }
      '{ new _root_.scala.xml.NamespaceBinding($prefix, $uri, $rest) }
    })
  }
  
  private def liftText(text: Text) = '{
    new _root_.scala.xml.Text(${text.text.toExpr})
  }
  
  private def liftComment(comment: Comment) = '{
    new _root_.scala.xml.Comment(${comment.text.toExpr})
  }
  
  private def liftPlaceholder(placeholder: Placeholder) given (args: Seq[Expr[Any]]) = {
    args(placeholder.id)
  }
  
  private def liftPCData(pcdata: PCData) = '{
    new _root_.scala.xml.PCData(${pcdata.data.toExpr})
  }
  
  private def liftProcInstr(instr: ProcInstr) = '{
    new _root_.scala.xml.ProcInstr(${instr.target.toExpr}, ${instr.proctext.toExpr})
  }
  
  private def liftEntityRef(ref: EntityRef) = '{
    new _root_.scala.xml.EntityRef(${ref.name.toExpr})
  }
  
  private def liftUnparsed(unparsed: Unparsed) = '{
    new _root_.scala.xml.Unparsed(${unparsed.data.toExpr})
  }
}