package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  def primary =
    new Alternates( List(
      IntegerRule,
      new Sequence( List(LeftParenRule, additive, RightParenRule), vec => vec(1) )
    ) )

  def additive: Rule = new LeftAssocBinary( primary, Set("+", "-") )

  val p = new Parser( additive )
  val input = "3 + 4 + 5"

  val ast = p( new StringReader(input) )

  println( ast )

}