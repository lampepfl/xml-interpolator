package dotty.xml.interpolator
package internal

import scala.util.parsing.input.*

given Conversion[Position, Int] with
  def apply(pos: Position): Int =
    pos match
      case OffsetPosition(_, offset) => offset
      case _ => throw new Exception("expected offset position")

private[internal] inline def ctx(using XmlContext): XmlContext = summon
private[internal] inline def reporter(using Reporter): Reporter = summon
