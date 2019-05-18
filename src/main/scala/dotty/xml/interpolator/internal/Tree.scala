package dotty.xml.interpolator
package internal

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input._

object Tree {
  sealed abstract class Node extends Positional
  final case class Group(nodes: Seq[Node]) extends Node
  final case class Elem(name: String, attributes: Seq[Attribute], empty: Boolean, children: Seq[Node]) extends Node {
    def prefix: String = name.take(prefixEnd)
    def label:  String = name.drop(prefixEnd + 1)
    private def prefixEnd = name.indexOf(':')
  }
  final case class Text(text: String) extends Node
  final case class Comment(text: String) extends Node
  final case class Placeholder(id: Int) extends Node
  final case class PCData(data: String) extends Node
  final case class ProcInstr(target: String, proctext: String) extends Node
  final case class EntityRef(name: String) extends Node
  final case class Unparsed(data: String) extends Node
  final case class Attribute(name: String, value: Seq[Node]) extends Positional {
    def prefix: String = name.take(prefixEnd) 
    def key:    String = name.drop(prefixEnd + 1)
    def isNamespace = name.startsWith("xmlns")
    private def prefixEnd = name.indexOf(':')
  }
}