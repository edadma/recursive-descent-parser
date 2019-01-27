package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.StringReader

import xyz.hyperreal.pretty._


object Main extends App {

  val grammarRef = new RuleRef[AST]
  val integer = new TokenClassRule( classOf[IntegerToken], (r, s) => IntegerAST(r, s.toInt), "expected integer" )
  val string = new TokenClassRule( classOf[StringToken], (r, s) => StringAST(r, s), "expected string" )
  val anyAtom =
    Alternates(
      List(
        new TokenClassRule( classOf[AtomToken], (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassRule( classOf[SymbolToken], (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassRule( classOf[QuotedAtomToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )
  val anyNonSymbolAtom =
    Alternates(
      List(
        new TokenClassRule( classOf[AtomToken], (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassRule( classOf[QuotedAtomToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )
  val primary =
    Alternates[AST]( List(
      integer,
      Sequence( List(Rule.symbol("("), grammarRef, Rule.symbol(")")), _(1).asInstanceOf[AST] ),
      Sequence( List(anyAtom, Rule.symbol("("), Rule.oneOrMoreSeparated(grammarRef, Rule.symbol(",")), Rule.symbol(")")), vec => StructureAST(vec(0).asInstanceOf[AtomAST].pos, vec(0).asInstanceOf[AtomAST].atom, vec(2).asInstanceOf[List[AST]]) ),
      //      Sequence( List(Rule.symbol("["), Rule.oneOrMoreSeparated(grammarRef, Rule.symbol(",")), Rule.symbol("]"), Optional(Sequence( List(Rule.symbol("|"), grammarRef)))), ),
      anyNonSymbolAtom
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