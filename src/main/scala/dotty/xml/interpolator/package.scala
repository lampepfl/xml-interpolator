package dotty.xml.interpolator

import scala.quoted._

type Scope = scala.xml.NamespaceBinding
implicit val top: Scope = scala.xml.TopScope

extension (inline ctx: StringContext) transparent inline def xml (inline args: (Scope ?=> Any)*)(using scope: Scope): Any =
  ${ dotty.xml.interpolator.internal.Macro.impl('ctx, 'args, 'scope) }
