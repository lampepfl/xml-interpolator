package dotty.xml.interpolator
package internal

import scala.language.implicitConversions
import scala.quoted.*
import dotty.xml.interpolator.internal.Tree.*

import scala.annotation.tailrec

object FillPlaceholders {

  def apply(nodes: Seq[Node], refNodes: Seq[scala.xml.Node])(using Quotes): Option[Map[Int, Any]] = {
    // TODO: handle single comment node
    if (nodes.size == 1) findInNode(nodes.head, refNodes.head)
    else findInNodes(nodes, refNodes)
  }

  private def findInNode(node: Node, refNode: scala.xml.Node)(using Quotes): Option[Map[Int, Any]] = {
    (node, refNode) match {
      case (group: Group, refGroup: scala.xml.Group)                 => findInGroup(group, refGroup)
      case (elem: Elem, refElem: scala.xml.Elem)                     => findInElem(elem, refElem)
      case (text: Text, refText: scala.xml.Text)                     => findInText(text, refText)
//      case (comment: Comment, refComment: scala.xml.Comment)         => expandComment(comment, refComment)
      case (placeholder: Placeholder, data)                          => fillPlaceholder(placeholder, data)
      case (pcData: PCData, refPCData: scala.xml.PCData)             => findInPCData(pcData, refPCData)
      case (procInstr: ProcInstr, refProcInstr: scala.xml.ProcInstr) => findInProcInstr(procInstr, refProcInstr)
      case (entityRef: EntityRef, refEntityRef: scala.xml.EntityRef) => findInEntityRef(entityRef, refEntityRef)
      case (unparsed: Unparsed, refUnparsed: scala.xml.Unparsed)     => findInUnparsed(unparsed, refUnparsed)
    }
  }

  private def findInNodes(nodes: Seq[Node], refNodes: Seq[scala.xml.Node])(using Quotes): Option[Map[Int, Any]] = {
    nodes
      .filter {
        case _: Comment => false
        case _ => true
      }
      .zip(refNodes.filter {
        case _: scala.xml.Comment => false
        case _ => true
      })
      .foldLeft(Some(Map()): Option[Map[Int, Any]]) {
        case (Some(map), (node, ref)) =>
          for fromNode <- findInNode(node, ref)
          yield map ++ fromNode
        case (None, _) =>
          return None
      }
  }

  private def findInGroup(group: Group, refGroup: scala.xml.Group)(using Quotes): Option[Map[Int, Any]] =
    findInNodes(group.nodes, refGroup.nodes)

  private def findInElem(elem: Elem, refElem: scala.xml.Elem)(using Quotes): Option[Map[Int, Any]] = {
//    // convert from linked list representation to a Seq of scala.xml.MetaData where each element's next is Null
//    @tailrec
//    def extractAttributesAndNamespaces(metadata: scala.xml.MetaData, attrs: Seq[scala.xml.MetaData] = Seq(), nss: Seq[scala.xml.MetaData] = Seq()): (Seq[scala.xml.MetaData], Seq[scala.xml.MetaData]) =
//      metadata match
//        case scala.xml.Null => (attrs, nss)
//        case scala.xml.PrefixedAttribute(pre, _, _, next) =>
//          val newAttr = metadata.copy(scala.xml.Null)
//          if pre.startsWith("xmlns") then
//            extractAttributesAndNamespaces(next, attrs, newAttr +: nss)
//          else
//            extractAttributesAndNamespaces(next, newAttr +: attrs, nss)
//        case scala.xml.UnprefixedAttribute(key, _, next) =>
//          val newAttr = metadata.copy(scala.xml.Null)
//          if key.startsWith("xmlns") then
//            extractAttributesAndNamespaces(next, attrs, newAttr +: nss)
//          else
//            extractAttributesAndNamespaces(next, newAttr +: attrs, nss)
     // convert from linked list representation to a Seq of scala.xml.MetaData where each element's next is Null
    @tailrec
    def extractAttributes(metadata: scala.xml.MetaData, attrs: Seq[scala.xml.Attribute] = Seq()): Seq[scala.xml.Attribute] =
      metadata match
        case scala.xml.Null => attrs
        case attr: scala.xml.Attribute => extractAttributes(attr.next, attr.copy(scala.xml.Null) +: attrs)

    val refAttributes = extractAttributes(refElem.attributes)

    if elem.prefix != refElem.prefix then
      return None

    if elem.label != refElem.label then
      return None

    for
      fromAttributes <- findInAttributes(elem.attributes, refAttributes)
      fromChildren <- findInNodes(elem.children, refElem.child)
    yield
      fromAttributes ++ fromChildren
  }

  private def findInAttributes(attributes: Seq[Attribute], refAttributes: Seq[scala.xml.Attribute])(using Quotes): Option[Map[Int, Any]] = {
    val attributes1 = attributes.sortBy(_.name)
    val refAttributes1 = refAttributes.sortBy { a =>
      if a.isPrefixed then s"${a.pre}:${a.key}" else a.key
    }

    attributes1
      .zip(refAttributes1)
      .foldLeft(Some(Map()): Option[Map[Int, Any]]) {
        case (Some(map), (attr, refAttr)) =>
          for fromNodes <- findInNodes(attr.value, refAttr.value)
          yield map ++ fromNodes
        case _ =>
          return None
      }
  }

  private def findInText(text: Text, refText: scala.xml.Text)(using Quotes): Option[Map[Int, Any]] =
    if text.text == refText.text then
      Some(Map())
    else
      None

//  private def expandComment(comment: Comment)(using Quotes): Expr[scala.xml.Comment] =
//    '{ new _root_.scala.xml.Comment(${Expr(comment.text)}) }

  private def fillPlaceholder(placeholder: Placeholder, data: scala.xml.Node)(using Quotes): Option[Map[Int, Any]] =
    Some(Map(placeholder.id -> data))

  private def findInPCData(pcdata: PCData, refPCData: scala.xml.PCData)(using Quotes): Option[Map[Int, Any]] =
    if pcdata.data == refPCData.data then Some(Map()) else None

  private def findInProcInstr(instr: ProcInstr, refInstr: scala.xml.ProcInstr)(using Quotes): Option[Map[Int, Any]] =
    if instr.target == refInstr.target then Some(Map()) else None

  private def findInEntityRef(ref: EntityRef, refRef: scala.xml.EntityRef)(using Quotes): Option[Map[Int, Any]] =
    if ref.name == refRef.entityName then Some(Map()) else None

  private def findInUnparsed(unparsed: Unparsed, refUnparsed: scala.xml.Unparsed)(using Quotes): Option[Map[Int, Any]] =
    if unparsed.data == refUnparsed.data then Some(Map()) else None
}
