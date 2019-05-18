package dotty.xml.interpolator
package internal

import scala.quoted._

class XmlContext(val args: Seq[Expr[given Scope => Any]], val scope: Expr[scala.xml.NamespaceBinding])