package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val grammarRef = new RuleRef
  val primary =
    new Alternates( List(
      IntegerRule,
      new Sequence( List(LeftParenRule, grammarRef, RightParenRule), vec => vec(1) )
    ) )
  val exponential: Rule = new RightAssocInfix( primary, Set("^") )
  val multiplicative: Rule = new LeftAssocInfix( exponential, Set("*", "/") )
  val additive: Rule = new LeftAssocInfix( multiplicative, Set("+", "-") )

  grammarRef.r = additive

  val p = new Parser( additive )
  val input = "2 - 3 + 4 * 5 ^ 6 ^ 7"

  val ast = p( new StringReader(input) )

  println( ast )

}