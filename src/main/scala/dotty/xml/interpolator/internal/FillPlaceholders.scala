package dotty.xml.interpolator
package internal

import scala.language.implicitConversions
import scala.quoted.*
import dotty.xml.interpolator.internal.Tree.*

import scala.annotation.tailrec
import scala.xml.Node.unapplySeq
import scala.xml.{NamespaceBinding, SpecialNode}

object FillPlaceholders {
  // Assumes that placeholder indices are in increasing order left to right
  def apply(nodes: Seq[Node], refNodes: Seq[scala.xml.Node]): Option[Seq[Any]] = {
    // remove comments
    val nodes1 = nodes.filter(!_.isInstanceOf[Comment])
    val refNodes1 = refNodes.filter(!_.isInstanceOf[scala.xml.Comment])

    if (nodes1.size == 1) findInNode(nodes1.head, refNodes1.head)
    else findInNodes(nodes1, refNodes1)
  }

  // string equality where (null == "")
  def strEq(s1: String, s2: String): Boolean =
    (if s1 == null then "" else s1) == (if s2 == null then "" else s2)

  // expects comments to be removed
  private def findInNode(node: Node, refNode: scala.xml.Node): Option[Seq[Any]] = {
    (node, refNode) match {
      case (group: Group, refGroup: scala.xml.Group)                 => findInGroup(group, refGroup)
      case (elem: Elem, refElem: scala.xml.Elem)                     => findInElem(elem, refElem)
      case (text: Text, refText: scala.xml.Text)                     => findInText(text, refText)
      case (placeholder: Placeholder, data)                          => fillPlaceholder(placeholder, data)
      case (pcData: PCData, refPCData: scala.xml.PCData)             => findInPCData(pcData, refPCData)
      case (procInstr: ProcInstr, refProcInstr: scala.xml.ProcInstr) => findInProcInstr(procInstr, refProcInstr)
      case (entityRef: EntityRef, refEntityRef: scala.xml.EntityRef) => findInEntityRef(entityRef, refEntityRef)
      case (unparsed: Unparsed, refUnparsed: scala.xml.Unparsed)     => findInUnparsed(unparsed, refUnparsed)

      // assertions
      case (_: Comment, _)           => throw IllegalStateException("comment passed as first argument to findInNode")
      case (_, _: scala.xml.Comment) => throw IllegalStateException("comment passed as second argument to findInNode")

      // non-matching nodes
      case _ => None
    }
  }

  private def findInNodes(nodes: Seq[Node], refNodes: Seq[scala.xml.Node]): Option[Seq[Any]] = {
    if nodes.length != refNodes.length then
      return None

    Some(nodes
      .filter {
        case _: Comment => false
        case _ => true
      }
      .zip(refNodes.filter {
        case _: scala.xml.Comment => false
        case _ => true
      })
      .foldLeft(Seq[Any]()) { case (seq, (node, ref)) =>
        findInNode(node, ref) match
          case None => return None
          case Some(fromNode) => seq ++ fromNode
      })
  }

  private def findInGroup(group: Group, refGroup: scala.xml.Group): Option[Seq[Any]] =
    findInNodes(group.nodes, refGroup.nodes)

  private def findInElem(elem: Elem, refElem: scala.xml.Elem): Option[Seq[Any]] = {
    // convert from linked list representation to a Seq of scala.xml.MetaData where each element's next is Null
    @tailrec
    def extractAttributes(metadata: scala.xml.MetaData, attrs: Seq[scala.xml.Attribute] = Seq()): Seq[scala.xml.Attribute] =
      metadata match
        case scala.xml.Null => attrs
        case attr: scala.xml.Attribute => extractAttributes(attr.next, attr.copy(scala.xml.Null) +: attrs)

    if !strEq(elem.prefix, refElem.prefix) then
      return None

    if !strEq(elem.label, refElem.label) then
      return None

    val refAttributes = extractAttributes(refElem.attributes)
    val refScope = refElem.scope

    for
      fromAttributes <- findInAttributes(elem.attributes, refElem)
      fromChildren <- findInNodes(elem.children, refElem.child)
    yield
      fromAttributes ++ fromChildren
  }

  private def findInAttributes(attributes: Seq[Attribute], refElem: scala.xml.Elem): Option[Seq[Any]] = {


    Some(attributes
      .foldLeft(Seq[Any]()) { (seq, attr) =>
        refElem.attributes.find(_.prefixedKey == attr.name) match
          case Some(refAttr) =>
            findInNodes(attr.value, refAttr.value) match
              case None => return None
              case Some(fromNodes) => seq ++ fromNodes
          case None if attr.prefix == "xmlns" =>
            findInBindings(attr.key, attr.value, refElem.scope) match
              case None => return None
              case Some(nb) => seq ++ nb
          case None if attr.prefix == "" && attr.key == "xmlns" =>
            findInBindings("", attr.value, refElem.scope) match
              case None => return None
              case Some(nb) => seq ++ nb
          case None =>
            return None
      })
  }

  private def findInBindings(key: String, value: Seq[Node], refBindings: scala.xml.NamespaceBinding): Option[Seq[String]] =
    extension (scope: NamespaceBinding) @tailrec def find(p: NamespaceBinding => Boolean): Option[NamespaceBinding] = {
      if scope == scala.xml.TopScope then
        None
      else if p(scope) then
        Some(scope)
      else
        scope.parent.find(p)
    }

    refBindings.find(nb => strEq(nb.prefix, key)) match
      case Some(refNsBinding) =>
        findInNsBinding(value, refNsBinding)
      case None =>
        None

  private def findInNsBinding(nodes: Seq[Node], refBinding: scala.xml.NamespaceBinding): Option[Seq[String]] =
    nodes match
      case Seq(Text(text)) =>
        if text == refBinding.uri then
          Some(Seq())
        else
          None
      case Seq(p@Placeholder(_)) =>
        fillPlaceholder(p, refBinding.uri)
      case _ => None

  private def findInText(text: Text, refText: scala.xml.Text): Option[Seq[Any]] =
    if text.text == refText.text then
      Some(Seq())
    else
      None

  private def fillPlaceholder(placeholder: Placeholder, data: scala.xml.Node | String): Option[Seq[data.type]] =
    Some(Seq(data))

  private def findInPCData(pcdata: PCData, refPCData: scala.xml.PCData): Option[Seq[Any]] =
    if pcdata.data == refPCData.data then Some(Seq()) else None

  private def findInProcInstr(instr: ProcInstr, refInstr: scala.xml.ProcInstr): Option[Seq[Any]] =
    if instr.target == refInstr.target then Some(Seq()) else None

  private def findInEntityRef(ref: EntityRef, refRef: scala.xml.EntityRef): Option[Seq[Any]] =
    if ref.name == refRef.entityName then Some(Seq()) else None

  private def findInUnparsed(unparsed: Unparsed, refUnparsed: scala.xml.Unparsed): Option[Seq[Any]] =
    if unparsed.data == refUnparsed.data then Some(Seq()) else None
}
