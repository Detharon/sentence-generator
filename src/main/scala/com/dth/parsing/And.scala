package com.dth.parsing

class And(val ors: Array[Or]) extends Expression {
  def this(or: Or) {
    this(Array(or))
  }

  def :++(newOr: Or): And = {
    new And(ors :+ newOr)
  }

  def :++(and: And): And = {
    new And(ors :++ and.ors)
  }

  override def toString: String = this.getClass.getSimpleName + "[" + ors.mkString(",") + "]"
  override def equals(obj: Any): Boolean = obj match {
    case and: And => and.ors sameElements ors
    case _ => false
  }
}