package dotty.xml.interpolator

import scala.quoted.*

type Scope = scala.xml.NamespaceBinding
implicit val top: Scope = scala.xml.TopScope

object XML:
  opaque type StringContext = scala.StringContext
  def apply(ctx: scala.StringContext): StringContext = ctx
  def unapply(ctx: StringContext): Option[scala.StringContext] = Some(ctx)

//extension (inline ctx: StringContext) transparent inline def xml (inline args: (Scope ?=> Any)*)(using scope: Scope): Any =
//  ${ dotty.xml.interpolator.internal.Macro.impl('ctx, 'args, 'scope) }

extension (ctx: StringContext) def xml: XML.StringContext = XML(ctx)

extension (inline ctx: XML.StringContext) transparent inline def apply(inline args: (Scope ?=> Any)*)(using scope: Scope): Any =
  ${ dotty.xml.interpolator.internal.Macro.impl('ctx, 'args, 'scope) }

extension (inline ctx: XML.StringContext) transparent inline def unapplySeq(inline elem: scala.xml.Node | scala.xml.NodeBuffer)(using scope: Scope): Option[Seq[Any]] =
  ${ dotty.xml.interpolator.internal.Macro.implUnapply('ctx, 'elem, 'scope) }
