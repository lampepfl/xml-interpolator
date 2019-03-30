package dotty.xml.interpolator

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
  object Attribute {

    def apply(name: String, value0: Either[String, Placeholder]): Attribute = {
      val value = value0 match {
        case Left(str) => normalize(str)
        case Right(placeholder) => Seq(placeholder)
      }
      Attribute(name, value)
    }

    private def normalize(value: String): Seq[Node] = {
      def ref(it: Iterator[Char]) = it.takeWhile(_ != ';').mkString

      val it = value.iterator.buffered
      val bf = new ListBuffer[Node]
      val sb = new StringBuilder

      def purgeText = {
        if (sb.nonEmpty) {
          bf += Text(sb.result())
          sb.clear
        }
      }

      while (it.hasNext) { it.next() } match {
        case ' ' | '\t' | '\n' | '\r' =>
          sb += ' '

        case '&' if it.head == '#' =>
          def charValueOf(cr: String, radix: Int = 10): Char = {
            if (cr.isEmpty) 0.toChar
            else java.lang.Integer.parseInt(cr, radix).toChar
          }
          it.next()
          val radix = if (it.head == 'x') { it.next(); 16 } else 10
          sb += charValueOf(ref(it), radix)

        case '&' =>
          val name = ref(it)
          unescape.get(name) match {
            case Some(c) =>
              sb += c
            case _ =>
              purgeText
              bf += EntityRef(name)
          }

        case c =>
          sb += c
      }

      purgeText
      bf.result
    }

    private val unescape = Map(
      "lt"    -> '<',
      "gt"    -> '>',
      "apos"  -> '\'',
      "quot"  -> '"',
      "quote" -> '"'
    )
  }
}