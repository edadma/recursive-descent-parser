package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val p = new Parser
  val input = "3 + 4 + 5"

  val ast = p( new StringReader(input), new LeftAssocBinary(IntegerRule, Set("+", "-")) )

  println( ast )

}