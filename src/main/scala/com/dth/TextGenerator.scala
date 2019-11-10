package com.dth

import com.dth.generator.StructuredTextGenerator
import com.dth.parsing.ExpressionParser
import org.parboiled2.ParseError

import scala.util.{Failure, Success}

object TextGenerator {
  def main(args: Array[String]) {
    val generator = new StructuredTextGenerator()
    val parser = new ExpressionParser(args(0))
    val result = parser.InputLine.run()
    result match {
      case Success(rel) =>
        println(rel)
        println(generator.generateRandom(rel))
      case Failure(pe: ParseError) =>println(parser.formatError(pe))
    }
  }
}