package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.{Reader, StringReader}
import xyz.hyperreal.pretty._

import scala.collection.mutable.ListBuffer


object Main extends App {

  val rule1200 = new RuleRef[AST]
  val rule900 = new RuleRef[AST]
  val integer = new TokenClassRule( _.isInstanceOf[IntegerToken], (r, s) => IntegerAST(r, s.toInt), "expected integer" )
  val string = new TokenClassRule( _.isInstanceOf[DoubleQuotedToken], (r, s) => StringAST(r, s), "expected string" )
  val cut = Action[(Reader, String), AtomAST]( Rule.symbol("!"), {case (pos, _) => AtomAST(pos, "!")} )
  val anyAtom =
    Alternates(
      List(
        new TokenClassRule( t => t.isInstanceOf[IdentToken] && t.value.head.isLower, (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassRule( _.isInstanceOf[SymbolToken], (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassRule( _.isInstanceOf[SingleQuotedToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )
  val variable = new TokenClassRule( t => t.isInstanceOf[IdentToken] && !t.value.head.isLower, {
    case (r, "_") => AnonymousAST( r )
    case (r, s) => VariableAST(r, s) }, "expected variable" )
  val anyNonSymbolAtom =
    Alternates(
      List(
        new TokenClassRule( t => t.isInstanceOf[IdentToken] && t.value.head.isLower, (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassRule( _.isInstanceOf[SingleQuotedToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )

  val primary =
    Alternates[AST]( List(
      cut,
      integer,
      string,
      variable,
      Rule.middle( Rule.symbol("("), rule1200, Rule.symbol(")") ),
      Sequence[AtomAST, List[AST], StructureAST](
        anyAtom,
        Rule.middle(
          Rule.symbol("("),
          Rule.oneOrMoreSeparated(rule900, Rule.symbol(",")),
          Rule.symbol(")")),
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
      Op(1000, 'xfy, ","),
      Op(900, 'fy, "\\+"),
      Op(700, 'xfx, "="),
      Op(700, 'xfx, "\\="),
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

  val expression = rules(1200)

  rule1200.ref = rules(1200)
  rule900.ref = rules(900)
  println( rules )

  val input = """ X is 1 + 2 """
  val parser = new Parser( rules(1200), ops ++ List("(", ")", ".", "[", "]", "|", "!") )
  println( "parsing..." )
  val ast = parser( new StringReader(input) )
  println( "done parsing" )

  println( prettyPrint(ast) )

//  val parser = new Parser( rules(1200), ops ++ List("(", ")", ".", "[", "]", "|", "!") )
//  val clauses = new ListBuffer[AST]
//
//  def clause( t: Stream[Token] ): Unit =
//    if (!t.head.isInstanceOf[EOIToken]) {
//      expression( t ) match {
//        case Success( rest, result ) =>
//          clauses += result
//
//          if (rest.head.value == ".")
//            clause( rest.tail )
//          else
//            sys.error( s"expected '.': ${rest.head}" )
//      }
//
//    }
//
//  clause( parser.tokenStream(Reader.fromFile("/home/ed/dev/prolog/examples/sudoku.prolog")) )
//  println( clauses.length )

  //  val integers = Rule.oneOrMoreSeparated( Rule.integer, Rule.atomMatch(",") )
  //  val p = new Parser( integers, List(",") )
  //  val input = "345 asdf"
  //
  //  val ast = p( new StringReader(input) )
  //
  //  println( ast )

}