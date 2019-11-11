package com.dth.parsing

case class Or(contents: Any*) extends Expression {
  def :++(or: Or): Or = {
    Or(contents :++ or.contents:_*)
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