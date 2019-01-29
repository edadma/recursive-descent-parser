package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.{Reader, StringReader}
import xyz.hyperreal.pretty._


object Main extends App {

  val rule1200 = new RuleRef[AST]
  val rule900 = new RuleRef[AST]
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
      Rule.middle( Rule.symbol("("), rule1200, Rule.symbol(")") ),
      Sequence[AtomAST, List[AST], StructureAST]( anyAtom, Rule.middle(Rule.symbol("("), Rule.oneOrMoreSeparated(rule900, Rule.symbol(",")), Rule.symbol(")")),
        (name, args) => StructureAST(name.pos, name.atom, args) ),
      anyNonSymbolAtom,
      SequenceLeft(
        Sequence[(Reader, String), (List[AST], Option[AST]), ListAST](
          Rule.symbol("["),
          Sequence[List[AST], Option[AST], (List[AST], Option[AST])](
            Rule.oneOrMoreSeparated(rule900, Rule.symbol(",")),
            Optional(SequenceRight(Rule.symbol("|"), rule1200)), (_, _) ), {case ((pos, _), (l, t)) => ListAST(pos, l, t)} ),
        Rule.symbol("]")),
      Sequence[(Reader, String), (Reader, String), AtomAST]( Rule.symbol("["), Rule.symbol("]"), (a, _) => AtomAST(a._1, "[]") )
    ) )
  val (rules, ops) = Builder[AST]( primary,
    List(
      Op(1200, 'xfx, ":-"),
      Op(1200, 'xfx, "-->"),
      Op(1200, 'fx, ":-"),
      Op(1200, 'fx, "?-"),
      Op(1100, 'xfy, ";"),
      Op(1050, 'xfy, "->"),
      Op(900, 'fy, "\\+"),
      Op(700, 'xfx, "="),
      Op( 700, 'xfx, "==" ),
      Op( 700, 'xfx, "\\==" ),
      Op( 700, 'xfx, "@<" ),
      Op( 700, 'xfx, "@=<" ),
      Op( 700, 'xfx, "@>" ),
      Op( 700, 'xfx, "@>=" ),
      Op( 700, 'xfx, "=.." ),
      Op( 700, 'xfx, "is" ),
      Op( 700, 'xfx, "=:=" ),
      Op( 700, 'xfx, "=\\=" ),
      Op( 700, 'xfx, "<" ),
      Op( 700, 'xfx, "=<" ),
      Op( 700, 'xfx, ">" ),
      Op( 700, 'xfx, ">=" ),
      Op(500, 'yfx, "+"),
      Op(500, 'yfx, "-"),
      Op(500, 'yfx, "/\\"),
      Op(500, 'yfx, "\\/"),
      Op(400, 'yfx, "*"),
      Op(400, 'yfx, "/"),
      Op(400, 'yfx, "//"),
      Op(400, 'yfx, "rem"),
      Op(400, 'yfx, "mod"),
      Op(400, 'yfx, "<<"),
      Op(400, 'yfx, ">>"),
      Op(200, 'xfx, "**"),
      Op(200, 'xfy, "^"),
      Op(200, 'fy, "-"),
      Op(200, 'fy, "\\")), (r, s, x) => StructureAST( r, s, List(x) ), (x, r, s, y) => StructureAST( r, s, List(x, y) ) )

  rule1200.ref = rules(1200)
  rule900.ref = rules(900)
  println( rules )

  val input = "3 + a(b, c)"
  val p = new Parser( rules(500), ops ++ List("(", ")", ",", ".", "[", "]", "|") )
  println( "parsing" )
  val ast = p( new StringReader(input) )

//  println( prettyPrint(ast) )
  println( ast )


  //  val integers = Rule.oneOrMoreSeparated( Rule.integer, Rule.atomMatch(",") )
  //  val p = new Parser( integers, List(",") )
  //  val input = "345 asdf"
  //
  //  val ast = p( new StringReader(input) )
  //
  //  println( ast )

}