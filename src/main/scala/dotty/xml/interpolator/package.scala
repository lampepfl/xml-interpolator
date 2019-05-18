package dotty.xml

import scala.quoted._

package object interpolator {

  type Scope = scala.xml.NamespaceBinding
  implicit val top: Scope = scala.xml.TopScope

  implicit object StringContextOps {
    inline def (ctx: => StringContext) xml (args: => (given Scope => Any)*) given Scope <: Any =
      ${ dotty.xml.interpolator.internal.Macro.impl('ctx, 'args, '{implicitly[Scope]}) }
  }
}