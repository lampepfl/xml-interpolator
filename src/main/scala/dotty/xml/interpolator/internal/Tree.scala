package dotty.xml.interpolator
package internal

import scala.util.parsing.input.*

object Tree:

  final case class Attribute(name: String, value: Seq[Node]) extends Positional {
    def prefix: String = name.take(prefixEnd) 
    def key:    String = name.drop(prefixEnd + 1)
    def isNamespace = name.startsWith("xmlns")
    private def prefixEnd = name.indexOf(':')
  }

  enum Node extends Positional:
    case Group(nodes: Seq[Node])
    case Elem(name: String, attributes: Seq[Attribute], children: Seq[Node], end: Option[String])
    case Text(text: String)
    case Comment(text: String)
    case Placeholder(id: Int)
    case PCData(data: String)
    case ProcInstr(target: String, proctext: String)
    case EntityRef(name: String)
    case Unparsed(data: String)
  end Node    

  export Node.*

  extension (elem: Node.Elem)
    private inline def prefixEnd = elem.name.indexOf(':')
    def prefix: String = elem.name.take(prefixEnd)
    def label:  String = elem.name.drop(prefixEnd + 1)

end Tree