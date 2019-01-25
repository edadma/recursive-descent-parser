package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val grammarRef = new RuleRef
  val primary =
    Alternates( List(
      IntegerRule,
      Sequence( List(LeftParenRule, grammarRef, RightParenRule), vec => vec(1) )
    ) )
//  val exponential: Rule = RightAssocInfix( primary, null, Set("^") )
//  val multiplicative: Rule = LeftAssocInfix( exponential, null, Set("*", "/") )
//  val additive: Rule = LeftAssocInfix( multiplicative, null, Set("+", "-") )
//
//  grammarRef.r = additive
//
//  val p = new Parser( additive )
//  val input = "2 - 3 + 4 * 5 ^ 6 ^ 7"
//
//  val ast = p( new StringReader(input) )

//  println( ast )

  val (grammar, ops) = Builder( primary, grammarRef, List(Op(500, 'yfx, "+"), Op(500, 'yfx, "-"), Op(400, 'yfx, "*"), Op(200, 'xfx, "**"), Op(200, 'xfy, "^"), Op(200, 'fx, "-")) )

  println( grammar )

  val input = "-3 ^ 4"
  val p = new Parser( grammar, ops )
  val ast = p( new StringReader(input) )

  println( ast )

}