package dotty.xml.interpolator

import scala.util.parsing.combinator._
import scala.xml.parsing.TokenTests

import scala.language.implicitConversions

class Parser extends JavaTokenParsers with TokenTests {
  import Hole._

  def XmlExpr: Parser[Seq[Tree.Node]] = XmlContent ~ Element.* ^^ { case elem ~ elems => elem :: elems }

  def Element: Parser[Tree.Node] = positioned(
      EmptyElemTag ^^ { case name ~ attributes => Tree.Elem(name, attributes, empty = true, Nil) }
    | STag ~ Content <~ ETag ^^ { case name ~ attributes ~ children => Tree.Elem(name, attributes, empty = false, children) }
  )
  
  def EmptyElemTag = "<" ~> Name ~ (S ~> Attribute).* <~ S.? <~ "/>" 

  def STag = "<"  ~> Name ~ (S ~> Attribute).* <~ S.? <~ ">"
  def ETag = "</" ~> Name <~ S.? <~ ">"
  def Content = (CharData.? ~ (Content1 ~ CharData.?).*) ^^ {
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
  def Content1 = (
      XmlContent
    | Reference
    | ScalaExpr
  )
  def XmlContent = (
      Unparsed
    | CDSect
    | PI
    | Comment
    | Element
  )

  def Attribute = positioned(Name ~ Eq ~ AttValue ^^ { case name ~ _ ~ value => Tree.Attribute(name, value) })

  def AttValue = (
      "\"" ~> (CharQ | CharRef).* <~ "\"" ^^ { case xs => Seq(Tree.Text(xs.map { case charq: String => charq; case text: Tree.Text => text.text }.mkString)) }
    | "'"  ~> (CharA | CharRef).* <~ "'"  ^^ { case xs => Seq(Tree.Text(xs.map { case charq: String => charq; case text: Tree.Text => text.text }.mkString)) }
    | ScalaExpr ^^ { expr => Seq(expr) }
  )  
  def ScalaExpr = Placeholder

  def Unparsed = positioned( UnpStart ~> UnpData <~ UnpEnd ^^ { case data => Tree.Unparsed(data) })
  def UnpStart = "<xml:unparsed" ~ (S ~ Attribute).* ~ S.? ~ ">"
  def UnpData  = (not(UnpEnd) ~> Char).*.map(_.mkString)
  def UnpEnd   = "</xml:unparsed>"

  def CharData: Parser[Tree.Node] = positioned(Char1.+ ^^ { case chars => Tree.Text(chars.mkString) })

  def Char  = not(Placeholder) ~> ".".r
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
  def EntityRef = "&" ~> Name <~ ";" ^^ { case name => Tree.EntityRef(name) }
  def CharRef   = (CharRef1 | CharRef2) ^^ { case text => Tree.Text(text) }
  def CharRef1  = "&#" ~ "[0-9]+".r ~ ";" ^^ { case ref ~ num ~ sc => ref ++ num ++ sc }
  def CharRef2  = "&#x" ~ "[0-9a-fA-F]+".r ~ ";" ^^ { case ref ~ hex ~ sc => ref ++ hex ++ sc }

  def CDSect: Parser[Tree.Node] = positioned(CDStart ~> CData <~ CDEnd ^^ { case data => Tree.PCData(data) })
  def CDStart = "<![CDATA["
  def CData   = (not("]]>") ~> Char).* ^^ { case xs => xs.mkString }
  def CDEnd   = "]]>"

  def PI: Parser[Tree.Node] = positioned("<?" ~> PITarget ~ PIProcText <~ "?>" ^^ { case target ~ proctext => Tree.ProcInstr(target, proctext) })
  def PITarget   = not(("X" | "x") ~ ("M" | "m") ~ ("L" | "l")) ~> Name
  def PIProcText = (S ~> (not("?>") ~> Char).*).? ^^ { proctext => proctext.getOrElse(List("")).mkString }

  def Comment: Parser[Tree.Node] = positioned("<!--" ~> (Comment1 | Comment2).* <~ "-->" ^^ { case xs => Tree.Comment(xs.mkString) })
  def Comment1 = not("-") ~> Char
  def Comment2 = "-" ~ (not("-") ~> Char) ^^ { case dash ~ char => dash ++ char }

  def S = acceptIf(isSpace)(_ => "whitespace")

  def Name = NameStart ~ (NameChar).* ^^ { case char ~ chars => (char :: chars).mkString }
  def NameStart = acceptIf(isNameStart)(_ => "NameStart")
  def NameChar  = acceptIf(isNameChar)(_ => "NameChar")

  def Eq = S.? ~ "=" ~ S.?

  def Placeholder: Parser[Tree.Node] = positioned(HoleStart ~> HoleChar.* ^^ { case chars => Tree.Placeholder(chars.length /* -1 ??? */) })
}