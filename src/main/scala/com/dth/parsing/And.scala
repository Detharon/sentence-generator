package com.dth.parsing

case class And(ors: Or*) extends Expression {
  def :++(newOr: Or): And = {
    And(ors :+ newOr:_*)
  }

  def :++(and: And): And = {
    And(ors :++ and.ors:_*)
  }

  override def toString: String = this.getClass.getSimpleName + "[" + ors.mkString(",") + "]"
  override def equals(obj: Any): Boolean = obj match {
    case and: And => and.ors sameElements ors
    case _ => false
  }
}