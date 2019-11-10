package com.dth.parsing

class Or(val contents: Array[Any]) extends Expression {
  def this(word: String) {
    this(Array(word.asInstanceOf[Any]))
  }

  def this(and: And) {
    this(Array(and.asInstanceOf[Any]))
  }

  def :++(or: Or): Or = {
    new Or(contents :++ or.contents)
  }

  def :++(and: And): Or = {
    new Or(contents :+ and)
  }

  override def toString: String = this.getClass.getSimpleName + "[" + contents.mkString(",") + "]"
  override def equals(obj: Any): Boolean = obj match {
    case or: Or => contents sameElements or.contents
    case _ => false
  }
}