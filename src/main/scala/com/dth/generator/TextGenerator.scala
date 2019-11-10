package com.dth.generator

import com.dth.parsing.And

trait TextGenerator {
  def generateAll(words: And): List[String]
  def generateRandom(words: And): String
}