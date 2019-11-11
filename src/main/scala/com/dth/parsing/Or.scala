package com.dth.parsing

case class Or(contents: Array[Any]) extends Expression {
  def :++(or: Or): Or = {
    Or(contents :++ or.contents)
  }

  def :++(and: And): Or = {
    Or(contents :+ and)
  }

  override def toString: String = this.getClass.getSimpleName + "[" + contents.mkString(",") + "]"
  override def equals(obj: Any): Boolean = obj match {
    case or: Or => contents sameElements or.contents
    case _ => false
  }
}

object Or {
  def apply(word: String): Or = new Or(Array(word.asInstanceOf[Any]))
  def apply(and: And): Or = new Or(Array(and.asInstanceOf[Any]))
}