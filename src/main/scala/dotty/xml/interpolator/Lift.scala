package dotty.xml.interpolator

import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.Exprs.LiftedExpr
import scala.quoted.Toolbox.Default._

import Tree._

object Lift {

  def apply(nodes: Seq[Node], args: List[Expr[Any]]): Expr[scala.xml.Node] = {
    /*if (nodes.size == 1) liftNode(nodes.head)(args)
    else liftNodes(nodes)(args)*/
    ???
  }

  private def liftNode(node: Node)(implicit args: List[Expr[Any]]): Expr[scala.xml.Node] = {
    node match {
      case group: Group             => liftGroup(group)
      //case elem: Elem               => liftElem(elem)
      case text: Text               => liftText(text)
      case comment: Comment         => liftComment(comment)
      //case placeholder: Placeholder => liftPlaceholder(placeholder)
      case pcData: PCData           => liftPCData(pcData)
      case procInstr: ProcInstr     => liftProcInstr(procInstr)
      case entityRef: EntityRef     => liftEntityRef(entityRef)
      case unparsed: Unparsed       => liftUnparsed(unparsed)
    }
  }

  private def liftNodes(nodes: Seq[Node])(implicit args: List[Expr[Any]]): Expr[scala.xml.NodeBuffer] = {
    nodes.foldRight('{ new _root_.scala.xml.NodeBuffer })((node, expr) => '{ ~expr &+ ~liftNode(node) } )
  }

  private def liftGroup(group: Group)(implicit args: List[Expr[Any]]) = '{
    new _root_.scala.xml.Group(~{liftNodes(group.nodes)})
  }

  private def liftElem(elem: Elem)(implicit args: List[Expr[Any]]) = {
    val (namespaces, attributes) = elem.attributes.partition(_.isNamespace)
    val prefix = if (elem.prefix.nonEmpty) elem.prefix.toExpr else '{ null: String }
    val label = elem.label.toExpr

    val empty = elem.empty.toExpr
    if (namespaces.isEmpty) {

    } else {

    }
  }

  private def liftAttributes(attributes: Seq[Attribute])(implicit args: List[Expr[Any]]): Expr[scala.xml.MetaData] = {
    attributes.foldRight('{ _root_.scala.xml.Null.asInstanceOf[scala.xml.MetaData] })((attr, rest) => {
      val value = attr.value match {
          case Seq(v) => liftNode(v)
          case vs     => liftNodes(vs)
      }
      val attribute = attr.prefix.isEmpty match {
        case true  => '{ new _root_.scala.xml.UnprefixedAttribute(~{attr.key.toExpr}, ~value, ~rest) }
        case false => '{ new _root_.scala.xml.PrefixedAttribute(~{attr.prefix.toExpr}, ~{attr.key.toExpr}, ~value, ~rest) }
      }
      attribute
    })
  }

  private def liftNamespaces(namespaces: Seq[Attribute])(implicit args: List[Expr[Any]]): Expr[scala.xml.NamespaceBinding] = {
    namespaces match {
      case n +: ns =>
        val prefix = if (n.prefix.nonEmpty) '{ ~{n.key.toExpr} } else '{ null: String }
        val uri = (n.value.head: @unchecked) match {
          case Text(text) => text.toExpr
          //case p : Placeholder => string staticaly known ?
        }
        '{ new _root_.scala.xml.NamespaceBinding(~prefix, ~uri, ~liftNamespaces(ns)) }
      case Seq() =>
        '{ ??? }
    }
  }
  
  private def liftText(text: Text) = '{
    new _root_.scala.xml.EntityRef(~{text.text.toExpr})
  }
  
  private def liftComment(comment: Comment) = '{
    new _root_.scala.xml.EntityRef(~{comment.text.toExpr})
  }
  
  /*private def liftPlaceholder(placeholder: Placeholder)(implicit args: List[Expr[Any]]) = '{
    args(placeholder.id)
  }*/
  
  private def liftPCData(pcdata: PCData) = '{
    new _root_.scala.xml.EntityRef(~{pcdata.data.toExpr})
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