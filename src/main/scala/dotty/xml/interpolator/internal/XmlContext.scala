package dotty.xml.interpolator
package internal

import scala.quoted.*

class XmlContext(val args: Seq[Expr[Scope ?=> Any]], val scope: Expr[Scope])
