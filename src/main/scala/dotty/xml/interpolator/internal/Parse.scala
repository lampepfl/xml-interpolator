package dotty.xml.interpolator
package internal

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import scala.xml.parsing.TokenTests

import scala.language.implicitConversions

object Parse extends JavaTokenParsers with TokenTests {
  import Hole._

  override val whiteSpace = "".r

  def apply(input: String)(using reporter: Reporter): Seq[Tree.Node] = {
    parseAll(XmlExpr, input) match {
      case Success(result, _) => result
      case failed : NoSuccess => reporter.error(failed.msg, failed.next.pos); Nil
    }
  }

  private def XmlExpr: Parser[Seq[Tree.Node]] = S.* ~> rep1sep(XmlContent, S.*) <~ S.*

  private def Element: Parser[Tree.Node] = positioned(
      EmptyElemTag ^^ { case name ~ attributes => Tree.Elem(name, attributes, Nil, None) }
    | STag ~ Content ~ ETag ^^ { case name ~ attributes ~ children ~ end => Tree.Elem(name, attributes, children, Some(end)) }
  )

  private def EmptyElemTag = "<" ~> Name ~ (S ~> Attribute).* <~ S.? <~ "/>"

  private def STag = "<"  ~> Name ~ (S ~> Attribute).* <~ S.? <~ ">"
  private def ETag = "</" ~> Name <~ S.? <~ ">"
  private def Content = (CharData | Reference | ScalaExpr | XmlContent).*
  private def XmlContent = (
      Unparsed
    | CDSect
    | PI
    | Comment
    | Element
  )

  private def Attribute = positioned(Name ~ Eq ~ AttValue ^^ { case name ~ _ ~ value => Tree.Attribute(name, value) }) ^^ { attribute =>
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

  private def AttValue = (
      "\"" ~> (CharQ | Reference1).* <~ "\"" ^^ { case texts => Seq(Tree.Text(texts.mkString)) }
    | "'"  ~> (CharA | Reference1).* <~ "'"  ^^ { case texts => Seq(Tree.Text(texts.mkString)) }
    | ScalaExpr ^^ { expr => Seq(expr) }
  )

  private def ScalaExpr = Placeholder

  private def Unparsed = positioned( UnpStart ~> UnpData <~ UnpEnd ^^ { case data => Tree.Unparsed(data) })
  private def UnpStart = "<xml:unparsed" ~ (S ~ Attribute).* ~ S.? ~ ">"
  private def UnpData  = (not(UnpEnd) ~> Char).*.map(_.mkString)
  private def UnpEnd   = "</xml:unparsed>"

  private def CharData: Parser[Tree.Node] = positioned(Char1.+ ^^ { case chars => Tree.Text(chars.mkString) })

  private def Char  = not(Placeholder) ~> "(?s).".r
  private def Char1 = not("<" | "&") ~> Char
  private def CharQ = not("\"") ~> Char1
  private def CharA = not("'")  ~> Char1

  private def XmlPattern: Parser[Tree.Node] = ElemPattern

  private def ElemPattern: Parser[Tree.Node] = positioned(
      EmptyElemP ^^ { case name => Tree.Elem(name, Nil, Nil, None) }
    | STagP ~ ContentP ~ ETagP ^^ { case name ~ children ~ end => Tree.Elem(name, Nil, children, Some(end)) }
  )

  private def EmptyElemP = "<" ~> Name <~ S.? ~ "/>"
  private def STagP = "<"  ~> Name <~ S.? ~ ">"
  private def ETagP = "</" ~> Name <~ S.? ~ ">"
  private def ContentP = (CharData.? ~ ((ElemPattern | ScalaPatterns) ~ CharData.?).*) ^^ {
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

  private def ScalaPatterns = ScalaExpr

  private def Reference: Parser[Tree.Node] = positioned(EntityRef | CharRef)
  private def EntityRef   = "&" ~> Name <~ ";" ^^ { case name => Tree.EntityRef(name) }
  private def CharRef     = ("&#" ~> Decimal <~ ";" | "&#x" ~> Hexadecimal <~ ";") ^^ { case number => Tree.Text(number.toString) }
  private def Decimal     = "[0-9]*".r ^^ { case str => if (str.isEmpty) 0.toChar else java.lang.Integer.parseInt(str).toChar }
  private def Hexadecimal = "[0-9a-fA-F]*".r ^^ { case str => if (str.isEmpty) 0.toChar else java.lang.Integer.parseInt(str, 16).toChar }

  private def Reference1 = EntityRef1 | CharRef1
  private def EntityRef1   = "&" ~> Name <~ ";" ^^ { case name => "&" + name + ";" }
  private def CharRef1     = (
      "&#" ~> Decimal1 <~ ";" ^^ { case decimal => "&#" + decimal + ";" }
    | "&#x" ~> Hexadecimal1 <~ ";" ^^ { case hexadecimal => "&#x" + hexadecimal + ";"  }
  )
  private def Decimal1     = "[0-9]*".r
  private def Hexadecimal1 = "[0-9a-fA-F]*".r

  private def CDSect: Parser[Tree.Node] = positioned(CDStart ~> CData <~ CDEnd ^^ { case data => Tree.PCData(data) })
  private def CDStart = "<![CDATA["
  private def CData   = (not("]]>") ~> Char).*.map(_.mkString)
  private def CDEnd   = "]]>"

  private def PI: Parser[Tree.Node] = positioned("<?" ~> Name ~ S.? ~ PIProcText <~ "?>" ^^ { case target ~ _ ~ text => Tree.ProcInstr(target, text) })
  private def PIProcText = (not("?>") ~> Char).*.map(_.mkString)

  private def Comment: Parser[Tree.Node] = positioned("<!--" ~> CommentText <~ "-->" ^^ { case text => Tree.Comment(text) })
  private def CommentText = (not("-->") ~> Char).*.map(_.mkString)

  private def S = acceptIf(isSpace)(c => s"unexpected '${c}' found")

  private def Name = NameStart ~ (NameChar).* ^^ { case char ~ chars => (char :: chars).mkString }
  private def NameStart = acceptIf(isNameStart)(c => s"'_' or a letter expected but '$c' found")
  private def NameChar  = acceptIf(isNameChar)(c => s"unexpected '${c}' found")

  private def Eq = S.? ~ "=" ~ S.?

  private def Placeholder = positioned(HoleStart ~ HoleChar.* ^^ { case char ~ chars => Tree.Placeholder((char :: chars).length -1) })
}
