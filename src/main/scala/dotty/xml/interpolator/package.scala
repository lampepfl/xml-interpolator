package dotty.xml

import scala.quoted._

package object interpolator {
  implicit object StringContextOps {
    inline def (ctx: => StringContext) xml (args: => Any*) <: Any =
      ${ dotty.xml.interpolator.internal.Macro.impl('ctx, 'args) }
  }
}