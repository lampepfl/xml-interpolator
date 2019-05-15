package dotty.xml.interpolator.internal

import scala.quoted._

class XmlContext(val args: Seq[Expr[Any]], val scope: Expr[scala.xml.NamespaceBinding])