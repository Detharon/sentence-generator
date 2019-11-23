package com.dth.parsing

import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {

  def RawText: Rule1[Or] = rule {
    capture(oneOrMore(CharPredicate.AlphaNum)) ~> ((string: String) => Or(string))
  }

  def TextParenthesis: Rule1[Or] = rule {
    '(' ~ RawText ~ ')'
  }

  def Text: Rule1[Or] = rule {
    RawText | TextParenthesis
  }

  def OrCombined: Rule1[Or] = rule {
    Text ~ zeroOrMore(
      '|' ~ Text ~> ((a: Or, b: Or) => a :++ b) |
        "|(" ~ AndText ~ ')' ~> ((a: Or, b: And) => a :++ b)
    )
  }

  def OrParenthesis: Rule1[Or] = rule {
    '(' ~ OrText ~ ')'
  }

  def OrText: Rule1[Or] = rule {
    OrCombined | OrParenthesis
  }

  def AndCombined: Rule1[And] = rule {
    OrText ~> ((a: Or) => new And(a)) ~ zeroOrMore(
      '&' ~ OrText ~> ((a: And, b: Or) => a :++ b) |
        "&(" ~ AndCombined ~ ")" ~> ((a: And, b: And)  => a :++ b)
    )
  }

  def AndParenthesis: Rule1[And] = rule {
    '(' ~ AndText ~ ')'
  }

  def AndText: Rule1[And] = rule {
    AndCombined | AndParenthesis
  }

  def InputLine: Rule1[And] = rule {
    AndText ~ EOI
  }
}