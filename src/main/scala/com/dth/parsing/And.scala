package com.dth.parsing

case class And(ors: Array[Or]) extends Expression {
  def :++(newOr: Or): And = {
    And(ors :+ newOr)
  }

  def :++(and: And): And = {
    And(ors :++ and.ors)
  }

  override def toString: String = this.getClass.getSimpleName + "[" + ors.mkString(",") + "]"
  override def equals(obj: Any): Boolean = obj match {
    case and: And => and.ors sameElements ors
    case _ => false
  }
}

object And {
  def apply(or: Or): And = And(Array(or))
}