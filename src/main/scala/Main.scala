package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader

import xyz.hyperreal.pretty._


object Main extends App {

  val grammarRef = new RuleRef[AST]
  val primary =
    Alternates[AST]( List(
      Rule.integer,
      Sequence( List(Rule.symbol("("), grammarRef, Rule.symbol(")")), _(1).asInstanceOf[AST] ),
      Sequence( List(Rule.anyAtom, Rule.symbol("("), Rule.oneOrMoreSeparated(grammarRef, Rule.symbol(",")), Rule.symbol(")")), vec => StructureAST(vec(0).asInstanceOf[AtomAST].pos, vec(0).asInstanceOf[AtomAST].atom, vec(2).asInstanceOf[ListAST].list) ),
      Rule.anyNonSymbolAtom
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


  val (grammar, ops) = Builder[AST]( primary, grammarRef,
    List(
      Op(500, 'yfx, "+"),
      Op(500, 'yfx, "-"),
      Op(400, 'yfx, "*"),
      Op(200, 'xfx, "**"),
      Op(200, 'xfy, "^"),
      Op(200, 'fx, "-")), UnaryAST, BinaryAST )

  println( grammar )

  val input = "3 + (b, c)"
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