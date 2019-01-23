package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader


object Main extends App {

  val p = new Parser
  val input = "123"

  val ast = p( new StringReader(input), p.IntegerRule )

  println( ast )

}