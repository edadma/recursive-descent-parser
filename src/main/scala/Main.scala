package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader

import xyz.hyperreal.pretty._


object Main extends App {

  val grammarRef = new RuleRef[AST]
  val primary =
    Alternates[AST]( List(
      Rule.integer,
      Sequence( List(Rule.symbol("("), grammarRef, Rule.symbol(")")), _(1).asInstanceOf[AST] ),
      Sequence( List(Rule.anyAtom, Rule.symbol("("), Rule.oneOrMoreSeparated(grammarRef, Rule.symbol(",")), Rule.symbol(")")), vec => StructureAST(vec(0).asInstanceOf[AtomAST].pos, vec(0).asInstanceOf[AtomAST].atom, vec(2).asInstanceOf[List[AST]]) ),
      Rule.anyNonSymbolAtom
    ) )
  val (grammar, ops) = Builder[AST]( primary, grammarRef,
    List(
      Op(500, 'yfx, "+"),
      Op(500, 'yfx, "-"),
      Op(400, 'yfx, "*"),
      Op(200, 'xfx, "**"),
      Op(200, 'xfy, "^"),
      Op(200, 'fx, "-")), (r, s, x) => StructureAST( r, s, List(x) ), (x, r, s, y) => StructureAST( r, s, List(x, y) ) )

  println( grammar )

  val input = "3 + -4 + 5"
  val p = new Parser( grammar, ops ++ List("(", ")", ",", ".", "[", "]") )
  val ast = p( new StringReader(input) )

  println( prettyPrint(ast) )


//  val integers = Rule.oneOrMoreSeparated( Rule.integer, Rule.atomMatch(",") )
//  val p = new Parser( integers, List(",") )
//  val input = "345 asdf"
//
//  val ast = p( new StringReader(input) )
//
//  println( ast )

}