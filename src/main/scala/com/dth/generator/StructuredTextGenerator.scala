package com.dth.generator

import com.dth.generator.StructuredTextGenerator.random
import com.dth.parsing.And

import scala.util.Random

class StructuredTextGenerator extends TextGenerator {
  override def generateAll(words: And): List[String] = ???
  override def generateRandom(words: And): String = {
    words.ors
      .map(orWords => randomWord(orWords.contents))
      .mkString(" ")
  }

  private def randomWord(expression: Seq[Any]): String = {
    val expressionOrWord = expression(random.nextInt(expression.length))

    expressionOrWord match {
      case word: String => word
      case expression:  And => generateRandom(expression)
    }
  }
}

object StructuredTextGenerator {
  def random: Random = new Random()
}