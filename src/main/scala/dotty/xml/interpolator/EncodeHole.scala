package dotty.xml.interpolator

import scala.collection.mutable.ArrayBuffer

object EncodeHole {

  def apply(ctx: StringContext): (String, Array[Int]) = {

    val sb = new StringBuilder()
    val bf = ArrayBuffer.empty[Int]

    def appendPart(part: String) = {
      bf += sb.length
      sb ++= part
      bf += sb.length
    }

    def appendHole(index: Int) = {
      sb ++= Hole.encode(index)
    }

    val parts = ctx.parts
    for ((part, index) <- parts.init.zipWithIndex) {
      appendPart(part)
      appendHole(index)
    }
    appendPart(parts.last)

    (sb.toString, bf.toArray)
  }
}