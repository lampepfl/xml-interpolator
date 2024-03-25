package dotty.xml.interpolator

import scala.quoted.*

private [interpolator] type Scope = scala.xml.NamespaceBinding

given Scope = scala.xml.TopScope

/** TODO: THIS NEED TO BE DOCUMENTED */
extension (inline ctx: StringContext)
  transparent inline def xml (inline args: (Scope ?=> Any)*)(using inline scope: Scope): Any =
    ${ internal.Macro.impl('ctx, 'args, 'scope) }
