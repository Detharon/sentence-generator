package com.dth.parsing

import org.parboiled2.ParseError
import org.scalatest.FlatSpec

import scala.util.{Failure, Success}

class ExpressionParserTest extends FlatSpec {

  // ==================================================
  // Single or
  // ==================================================

  it should "parse a|b" in {
    val result = parse("a|b")
    assert(result == And(Or("a", "b")))
  }

  it should "parse (a|b)" in {
    val result = parse("(a|b)")
    assert(result == And(Or("a", "b")))
  }

  it should "parse (((a|b)))" in {
    val result = parse("(((a|b)))")
    assert(result == And(Or("a", "b")))
  }

  it should "parse (a)|b" in {
    val result = parse("(a)|b")
    assert(result == And(Or("a", "b")))
  }

  it should "parse a|(b)" in {
    val result = parse("a|(b)")
    assert(result == And(Or("a", "b")))
  }

  it should "parse (a)|(a)" in {
    val result = parse("(a)|(b)")
    assert(result == And(Or("a", "b")))
  }

  it should "parse ((a)|(a))" in {
    val result = parse("((a)|(b))")
    assert(result == And(Or("a", "b")))
  }

  it should "parse (((a)|(a)))" in {
    val result = parse("(((a)|(b)))")
    assert(result == And(Or("a", "b")))
  }

  // ==================================================
  // Single and
  // ==================================================

  it should "parse a&b" in {
    val result = parse("a&b")
    assert(result == And(Or("a"), Or("b")))
  }

  it should "parse (a&b)" in {
    val result = parse("(a&b)")
    assert(result == And(Or("a"), Or("b")))
  }

  it should "parse (((a&b)))" in {
    val result = parse("(((a&b)))")
    assert(result == And(Or("a"), Or("b")))
  }

  it should "parse (a)&b" in {
    val result = parse("(a)&b")
    assert(result == And(Or("a"), Or("b")))
  }

  it should "parse a&(b)" in {
    val result = parse("a&(b)")
    assert(result == And(Or("a"), Or("b")))
  }

  it should "parse (a)&(b)" in {
    val result = parse("(a)&(b)")
    assert(result == And(Or("a"), Or("b")))
  }

  it should "parse ((a)&(b))" in {
    val result = parse("((a)&(b))")
    assert(result == And(Or("a"), Or("b")))
  }

  it should "parse (((a)&(b)))" in {
    val result = parse("(((a)&(b)))")
    assert(result == And(Or("a"), Or("b")))
  }

  // ==================================================
  // Test utils
  // ==================================================

  private def parse(text: String): And = {
    val parser: ExpressionParser = new ExpressionParser(text)

    parser.InputLine.run() match {
      case Success(and: And) => and
      case Failure(error: ParseError) =>
        println(parser.formatError(error))
        throw new Exception
    }
  }
}