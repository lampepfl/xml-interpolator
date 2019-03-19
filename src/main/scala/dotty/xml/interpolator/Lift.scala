package dotty.xml.interpolator

import scala.language.implicitConversions
import scala.quoted._
import scala.quoted.Exprs.LiftedExpr
import scala.quoted.Toolbox.Default._
import scala.tasty.Reflection

import Tree._

object Lift {

  def apply(nodes: Seq[Node], args: List[Expr[Any]]): Expr[xml.Node | xml.NodeBuffer] = {
    val scope = '{ _root_.scala.xml.TopScope }
    if (nodes.size == 1) liftNode(nodes.head)(args, scope)
    else liftNodes(nodes)(args, scope)
  }

  private def liftNode(node: Node)(implicit args: List[Expr[Any]], outer: Expr[scala.xml.NamespaceBinding]): Expr[scala.xml.Node] = {
    node match {
      case group: Group             => liftGroup(group)
      case elem: Elem               => liftElem(elem)
      case text: Text               => liftText(text)
      case comment: Comment         => liftComment(comment)
      //case placeholder: Placeholder => liftPlaceholder(placeholder)
      case pcData: PCData           => liftPCData(pcData)
      case procInstr: ProcInstr     => liftProcInstr(procInstr)
      case entityRef: EntityRef     => liftEntityRef(entityRef)
      case unparsed: Unparsed       => liftUnparsed(unparsed)
    }
  }

  private def liftNodes(nodes: Seq[Node])(implicit args: List[Expr[Any]], outer: Expr[scala.xml.NamespaceBinding]): Expr[scala.xml.NodeBuffer] = {
    nodes.foldRight('{ new _root_.scala.xml.NodeBuffer() })((node, expr) => '{ ~expr &+ ~liftNode(node) } )
  }

  private def liftGroup(group: Group)(implicit args: List[Expr[Any]], outer: Expr[scala.xml.NamespaceBinding]) = '{
    new _root_.scala.xml.Group(~{liftNodes(group.nodes)})
  }

  private def liftElem(elem: Elem)(implicit args: List[Expr[Any]], outer: Expr[scala.xml.NamespaceBinding]) = {
    val (namespaces, attributes) = elem.attributes.partition(_.isNamespace)
    val prefix = if (elem.prefix.nonEmpty) elem.prefix.toExpr else '{ null: String }
    val label = elem.label.toExpr
    val attributes1 = liftAttributes(attributes)
    val scope = liftNamespaces(namespaces)(args, outer)
    val empty = elem.empty.toExpr
    val child = liftNodes(elem.children)(args, scope)
    if (elem.children.isEmpty)
      '{ new _root_.scala.xml.Elem(~prefix, ~label, ~attributes1, ~scope, ~empty) }
    else
      '{ new _root_.scala.xml.Elem(~prefix, ~label, ~attributes1, ~scope, ~empty, ~child: _*) }
  }

  private def liftAttributes(attributes: Seq[Attribute])(implicit args: List[Expr[Any]], outer: Expr[scala.xml.NamespaceBinding]): Expr[scala.xml.MetaData] = {
    attributes.foldRight('{ _root_.scala.xml.Null }: Expr[scala.xml.MetaData])((attr, rest) => {
      val value = attr.value match {
          case Seq(v) => liftNode(v)
          case vs     => liftNodes(vs)
      }
      if (attr.prefix.isEmpty)
        '{ new _root_.scala.xml.UnprefixedAttribute(~{attr.key.toExpr}, ~value, ~rest) }
      else
        '{ new _root_.scala.xml.PrefixedAttribute(~{attr.prefix.toExpr}, ~{attr.key.toExpr}, ~value, ~rest) }
    })
  }

  private def liftNamespaces(namespaces: Seq[Attribute])(implicit args: List[Expr[Any]], outer: Expr[scala.xml.NamespaceBinding]): Expr[scala.xml.NamespaceBinding] = {
    namespaces.foldRight('{ ~outer })((ns, rest) => {
      val prefix = if (ns.prefix.nonEmpty) '{ ~{ns.key.toExpr} } else '{ null: String }
      val uri = (ns.value.head: @unchecked) match {
        case Text(text) => text.toExpr
        case Placeholder(id) => args(id).asInstanceOf[String].toExpr
      }
      '{ new _root_.scala.xml.NamespaceBinding(~prefix, ~uri, ~rest) }
    })
  }
  
  private def liftText(text: Text) = '{
    new _root_.scala.xml.Text(~{text.text.toExpr})
  }
  
  private def liftComment(comment: Comment) = '{
    new _root_.scala.xml.Comment(~{comment.text.toExpr})
  }
  
  private def liftPlaceholder(placeholder: Placeholder)(implicit args: List[Expr[Any]]) = {
    args(placeholder.id)
  }
  
  private def liftPCData(pcdata: PCData) = '{
    new _root_.scala.xml.PCData(~{pcdata.data.toExpr})
  }
  
  private def liftProcInstr(instr: ProcInstr) = '{
    new _root_.scala.xml.ProcInstr(~{instr.target.toExpr}, ~{instr.proctext.toExpr})
  }
  
  private def liftEntityRef(ref: EntityRef) = '{
    new _root_.scala.xml.EntityRef(~{ref.name.toExpr})
  }
  
  private def liftUnparsed(unparsed: Unparsed) = '{
    new _root_.scala.xml.Unparsed(~{unparsed.data.toExpr})
  }
}