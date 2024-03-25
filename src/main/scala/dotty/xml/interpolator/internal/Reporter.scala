package dotty.xml.interpolator
package internal

import scala.quoted.*

trait Reporter:
  def error(msg: String, idx: Int): Unit
  def error(msg: String, expr: Expr[Any]): Unit

object Reporter:
  def from(idx: Int, offsets: Array[Int], parts: Seq[Expr[String]]): (Expr[String], Int) = {
    val index = offsets.lastIndexWhere(idx >= _)
    val isWithinHoleOrAtTheEnd = index % 2 != 0
    val (partIndex, offset) = isWithinHoleOrAtTheEnd match {
      case true  => ((index - 1) / 2, idx - offsets(index - 1))
      case false => (index / 2, idx - offsets(index))
    }
    (parts(partIndex), offset)
  }