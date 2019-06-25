package dotty.xml.interpolator.internal

import scala.util.parsing.input._

delegate for Conversion[Position, Int] {
  def apply(pos: Position): Int = {
    pos match {
      case OffsetPosition(_, offset) => offset
      case _ => throw new Exception("expected offset position")
    }
  }
}