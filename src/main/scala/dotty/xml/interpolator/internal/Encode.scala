package dotty.xml.interpolator.internal

import scala.collection.mutable.ArrayBuffer

object Encode {
  def apply(parts: Seq[String]): (String, Array[Int]) = {

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

    for ((part, index) <- parts.init.zipWithIndex) {
      appendPart(part)
      appendHole(index)
    }
    appendPart(parts.last)

    (sb.toString, bf.toArray)
  }
}