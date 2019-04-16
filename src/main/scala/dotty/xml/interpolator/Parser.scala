package dotty.xml.interpolator

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import scala.xml.parsing.TokenTests

import scala.language.implicitConversions

class Parser extends JavaTokenParsers with TokenTests {
  import Hole._

  override val whiteSpace = "".r

  def XmlExpr: Parser[Seq[Tree.Node]] = S.* ~> rep1sep(XmlContent, S.*) <~ S.*

  def Element: Parser[Tree.Node] = positioned(
      EmptyElemTag ^^ { case name ~ attributes => Tree.Elem(name, attributes, empty = true, Nil) }
    | STag ~ Content <~ ETag ^^ { case name ~ attributes ~ children => Tree.Elem(name, attributes, empty = false, children) }
  )
  
  def EmptyElemTag = "<" ~> Name ~ (S ~> Attribute).* <~ S.? <~ "/>" 

  def STag = "<"  ~> Name ~ (S ~> Attribute).* <~ S.? <~ ">"
  def ETag = "</" ~> Name <~ S.? <~ ">"
  def Content = (CharData | Reference | ScalaExpr | XmlContent).*
  def XmlContent = (
      Unparsed
    | CDSect
    | PI
    | Comment
    | Element
  )

  def Attribute = positioned(Name ~ Eq ~ AttValue ^^ { case name ~ _ ~ value => Tree.Attribute(name, value) }) ^^ { attribute =>
    def normalize(value: String, position: Position) = {
      def ref(it : Iterator[Char]) = it.takeWhile(_ != ';').mkString

      val attrUnescape = Map(
        "lt"    -> '<',
        "gt"    -> '>',
        "apos"  -> '\'',
        "quot"  -> '"',
        "quote" -> '"'
      )

      val it = value.iterator.buffered
      val bf = new ListBuffer[Tree.Node]
      val sb = new StringBuilder

      val pos = position.asInstanceOf[OffsetPosition]
      val source = pos.source
      var offset = pos.offset

      def purgeText() = {
        if (sb.nonEmpty) {
          bf += Tree.Text(sb.result()).setPos(newPosition())
          sb.clear()
        }
      }

      def newPosition() = {
        OffsetPosition(source, offset)
      }

      while (it.hasNext) { offset += 1; it.next() } match {
        case ' ' | '\t' | '\n' | '\r' =>
          sb += ' '

        case '&' if it.head == '#' =>
          it.next()
          val radix =
            if (it.head == 'x') { it.next(); 16 }
            else 10
          val str = ref(it)
          sb += (if (str.isEmpty) 0.toChar else java.lang.Integer.parseInt(str, radix).toChar)

        case '&' =>
          val name = ref(it)
          attrUnescape.get(name) match {
            case Some(c) =>
              sb += c
            case _ =>
              purgeText()
              bf += Tree.EntityRef(name).setPos(newPosition())
          }

        case c =>
          sb += c

      }

      purgeText()
      bf.result()
    }
    attribute.value match {
      case Seq(placeholder : Tree.Placeholder) => attribute
      case Seq(text : Tree.Text) => attribute.copy(value = normalize(text.text, attribute.pos)).setPos(attribute.pos)
    }
  }

  def AttValue = (
      "\"" ~> (CharQ | Reference1).* <~ "\"" ^^ { case texts => Seq(Tree.Text(texts.mkString)) }
    | "'"  ~> (CharA | Reference1).* <~ "'"  ^^ { case texts => Seq(Tree.Text(texts.mkString)) }
    | ScalaExpr ^^ { expr => Seq(expr) }
  )

  def ScalaExpr = Placeholder

  def Unparsed = positioned( UnpStart ~> UnpData <~ UnpEnd ^^ { case data => Tree.Unparsed(data) })
  def UnpStart = "<xml:unparsed" ~ (S ~ Attribute).* ~ S.? ~ ">"
  def UnpData  = (not(UnpEnd) ~> Char).*.map(_.mkString)
  def UnpEnd   = "</xml:unparsed>"

  def CharData: Parser[Tree.Node] = positioned(Char1.+ ^^ { case chars => Tree.Text(chars.mkString) })

  def Char  = not(Placeholder) ~> "(?s).".r
  def Char1 = not("<" | "&") ~> Char
  def CharQ = not("\"") ~> Char1
  def CharA = not("'")  ~> Char1

  def XmlPattern: Parser[Tree.Node] = ElemPattern

  def ElemPattern: Parser[Tree.Node] = positioned(
      EmptyElemP ^^ { case name => Tree.Elem(name, Nil, empty = true, Nil) }
    | STagP ~ ContentP <~ ETagP ^^ { case name ~ children => Tree.Elem(name, Nil, empty = false, children) }
  )

  def EmptyElemP = "<" ~> Name <~ S.? ~ "/>"
  def STagP = "<"  ~> Name <~ S.? ~ ">"
  def ETagP = "</" ~> Name <~ S.? ~ ">"
  def ContentP = (CharData.? ~ ((ElemPattern | ScalaPatterns) ~ CharData.?).*) ^^ {
    case x ~ xs => {
      val nodes = xs.flatMap {
        case node ~ Some(chardata) => List(node, chardata)
        case node ~ _ => List(node)
      }
      x match {
        case Some(chardata) => chardata :: nodes
        case _ => nodes
      }
    }
  }

  def ScalaPatterns = ScalaExpr

  def Reference: Parser[Tree.Node] = positioned(EntityRef | CharRef)
  def EntityRef   = "&" ~> Name <~ ";" ^^ { case name => Tree.EntityRef(name) }
  def CharRef     = ("&#" ~> Decimal <~ ";" | "&#x" ~> Hexadecimal <~ ";") ^^ { case number => Tree.Text(number.toString) }
  def Decimal     = "[0-9]*".r ^^ { case str => if (str.isEmpty) 0.toChar else java.lang.Integer.parseInt(str).toChar }
  def Hexadecimal = "[0-9a-fA-F]*".r ^^ { case str => if (str.isEmpty) 0.toChar else java.lang.Integer.parseInt(str, 16).toChar }

  def Reference1 = EntityRef1 | CharRef1
  def EntityRef1   = "&" ~> Name <~ ";" ^^ { case name => "&" + name + ";" }
  def CharRef1     = (
      "&#" ~> Decimal1 <~ ";" ^^ { case decimal => "&#" + decimal + ";" }
    | "&#x" ~> Hexadecimal1 <~ ";" ^^ { case hexadecimal => "&#x" + hexadecimal + ";"  }
  )
  def Decimal1     = "[0-9]*".r
  def Hexadecimal1 = "[0-9a-fA-F]*".r

  def CDSect: Parser[Tree.Node] = positioned(CDStart ~> CData <~ CDEnd ^^ { case data => Tree.PCData(data) })
  def CDStart = "<![CDATA["
  def CData   = (not("]]>") ~> Char).*.map(_.mkString)
  def CDEnd   = "]]>"

  def PI: Parser[Tree.Node] = positioned("<?" ~> Name ~ S.? ~ PIProcText <~ "?>" ^^ { case target ~ _ ~ text => Tree.ProcInstr(target, text) })
  def PIProcText = (not("?>") ~> Char).*.map(_.mkString)

  def Comment: Parser[Tree.Node] = positioned("<!--" ~> CommentText <~ "-->" ^^ { case text => Tree.Comment(text) })
  def CommentText = (not("-->") ~> Char).*.map(_.mkString)

  def S = acceptIf(isSpace)(_ => "whitespace")

  def Name = NameStart ~ (NameChar).* ^^ { case char ~ chars => (char :: chars).mkString }
  def NameStart = acceptIf(isNameStart)(_ => "NameStart")
  def NameChar  = acceptIf(isNameChar)(_ => "NameChar")

  def Eq = S.? ~ "=" ~ S.?

  def Placeholder = positioned(HoleStart ~ HoleChar.* ^^ { case char ~ chars => Tree.Placeholder((char :: chars).length -1) })
}