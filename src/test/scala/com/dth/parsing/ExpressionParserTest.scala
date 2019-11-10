package com.dth.parsing

import org.parboiled2.ParseError
import org.scalatest.FlatSpec

import scala.util.{Failure, Success}

class ExpressionParserTest extends FlatSpec {

  it should "parse a|a" in {
    val result = checkThatSucceeded(new ExpressionParser("a|b"))
    assert(result == new And(Array(new Or(Array[Any]("a", "b")))))
  }

  it should "parse a&b" in {
    val result = checkThatSucceeded(new ExpressionParser("a&b"))
    assert(result == new And(Array(new Or("a"), new Or("b"))))
  }

  private def checkThatSucceeded(parser: ExpressionParser): And = {
    parser.InputLine.run() match {
      case Success(and: And) => and
      case Failure(error: ParseError) =>
        println(parser.formatError(error))
        throw new Exception
    }
  }
}