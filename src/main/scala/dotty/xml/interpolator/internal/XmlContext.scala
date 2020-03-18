package dotty.xml.interpolator
package internal

import scala.quoted._

class XmlContext(val args: Seq[Expr[Scope ?=> Any]], val scope: Expr[scala.xml.NamespaceBinding])
