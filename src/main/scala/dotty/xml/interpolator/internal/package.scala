package dotty.xml.interpolator.internal

import scala.util.parsing.input._

given Conversion[Position, Int] with {
  def apply(pos: Position): Int = {
    pos match {
      case OffsetPosition(_, offset) => offset
      case _ => throw new Exception("expected offset position")
    }
  }
}
