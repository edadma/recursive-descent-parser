package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val grammarRef = new RuleRef
  val primary =
    new Alternates( List(
      IntegerRule,
      new Sequence( List(LeftParenRule, grammarRef, RightParenRule), vec => vec(1) )
    ) )
  val multiplicative: Rule = new LeftAssocInfix( primary, Set("*", "/") )
  val additive: Rule = new LeftAssocInfix( multiplicative, Set("+", "-") )

  grammarRef.r = additive

  val p = new Parser( additive )
  val input = "3 + 4 * 5"

  val ast = p( new StringReader(input) )

  println( ast )

}