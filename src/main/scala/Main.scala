package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader

import xyz.hyperreal.pretty._


object Main extends App {

  val grammarRef = new RuleRef
  val primary =
    Alternates( List(
      Rule.integer,
      Sequence( List(Rule.leftParen, grammarRef, Rule.rightParen), vec => vec(1) ),
//      Sequence( List(Rule.atom, Rule.leftParen, Rule.rightParen), vec => vec(1) ),
      Rule.atom
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

  val input = "a - -(b + c)"
  val p = new Parser( grammar, ops ++ List("(", ")", ",", ".", "[", "]") )
  val ast = p( new StringReader(input) )

  println( prettyPrint(ast) )

}