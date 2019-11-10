package com.dth.parsing

import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {

  def Word: Rule1[Or] = rule {
    capture(oneOrMore(CharPredicate.AlphaNum)) ~> ((string: String) => new Or(string))
  }

  def Words: Rule1[Or] = rule {
    Word ~ zeroOrMore(
      '|' ~ Word ~> ((a: Or, b: Or) => a :++ b) |
        "|(" ~ And ~ ')' ~> ((a: Or, b: And) => a :++ b)
    )
  }

  def And: Rule1[And] = rule {
    Words ~> ((a: Or) => new And(Array(a))) ~ zeroOrMore(
      '&' ~ Words ~> ((a: And, b: Or) => a :++ b) |
        "&(" ~ And ~ ")" ~> ((a: And, b: And)  => a :++ b)
    )
  }

  def InputLine: Rule1[And] = rule {
    And ~ EOI
  }
}