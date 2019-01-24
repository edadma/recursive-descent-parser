package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  lazy val primary =
    new Alternates( List(
      IntegerRule,
      new Sequence( List(LeftParenRule, new LazyRule(additive), RightParenRule), vec => vec(1) )
    ) )

  lazy val additive: Rule = new LeftAssocBinary( primary, Set("+", "-") )

  val p = new Parser( additive )
  val input = "3 + (4 + 5)"

  val ast = p( new StringReader(input) )

  println( ast )

}