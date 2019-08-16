package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.{Reader, StringReader}
import xyz.hyperreal.pretty._

import scala.collection.mutable.ListBuffer


object Main extends App {

  val rule1200 = new ParserRef[AST]
  val rule900 = new ParserRef[AST]
  val integer = new TokenClassParser( _.isInstanceOf[IntegerToken], (r, s) => IntegerAST(r, s.toInt), "expected integer" )
  val string = new TokenClassParser( _.isInstanceOf[DoubleQuotedToken], (r, s) => StringAST(r, s), "expected string" )
  val cut = Action[(Reader, String), AtomAST]( Parser.symbol("!"), {case (pos, _) => AtomAST(pos, "!")} )
  val anyAtom =
    Alternates(
      List(
        new TokenClassParser( t => t.isInstanceOf[IdentToken] && t.value.head.isLower, (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassParser( _.isInstanceOf[SymbolToken], (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassParser( _.isInstanceOf[SingleQuotedToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )
  val variable = new TokenClassParser( t => t.isInstanceOf[IdentToken] && !t.value.head.isLower, {
    case (r, "_") => AnonymousAST( r )
    case (r, s) => VariableAST(r, s) }, "expected variable" )
  val anyNonSymbolAtom =
    Alternates(
      List(
        new TokenClassParser(t => t.isInstanceOf[IdentToken] && t.value.head.isLower, (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassParser( _.isInstanceOf[SingleQuotedToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )

  val primary =
    Alternates[AST]( List(
      cut,
      integer,
      string,
      variable,
      Parser.middle( Parser.symbol("("), rule1200, Parser.symbol(")") ),
      Sequence[AtomAST, List[AST], StructureAST](
        anyAtom,
        Parser.middle(
          Parser.symbol("("),
          Parser.oneOrMoreSeparated(rule900, Parser.symbol(",")),
          Parser.symbol(")")),
        (name, args) => StructureAST(name.pos, name.atom, args) ),
      anyNonSymbolAtom,
      SequenceLeft(
        Sequence[(Reader, String), (List[AST], Option[AST]), ListAST](
          Parser.symbol("["),
          Sequence[List[AST], Option[AST], (List[AST], Option[AST])](
            Parser.oneOrMoreSeparated(rule900, Parser.symbol(",")),
            Optional(SequenceRight(Parser.symbol("|"), rule1200)), (_, _) ), {case ((pos, _), (l, t)) => ListAST(pos, l, t)} ),
        Parser.symbol("]")),
      Sequence[(Reader, String), (Reader, String), AtomAST]( Parser.symbol("["), Parser.symbol("]"), (a, _) => AtomAST(a._1, "[]") )
    ) )
  val (rules, ops) = Builder[AST]( primary,
    List(
      Op( 1200, XFX, ":-" ),
      Op( 1200, XFX, "-->" ),
      Op( 1200, FX, ":-" ),
      Op( 1200, FX, "?-" ),
      Op( 1100, XFY, ";" ),
      Op( 1050, XFY, "->" ),
      Op( 1000, XFY, "," ),
      Op( 900, FY, "\\+" ),
      Op( 700, XFX, "=" ),
      Op( 700, XFX, "\\=" ),
      Op( 700, XFX, "==" ),
      Op( 700, XFX, "\\==" ),
      Op( 700, XFX, "@<" ),
      Op( 700, XFX, "@=<" ),
      Op( 700, XFX, "@>" ),
      Op( 700, XFX, "@>=" ),
      Op( 700, XFX, "=.." ),
      Op( 700, XFX, "is" ),
      Op( 700, XFX, "=:=" ),
      Op( 700, XFX, "=\\=" ),
      Op( 700, XFX, "<" ),
      Op( 700, XFX, "=<" ),
      Op( 700, XFX, ">" ),
      Op( 700, XFX, ">=" ),
      Op( 500, YFX, "+" ),
      Op( 500, YFX, "-" ),
      Op( 500, YFX, "/\\" ),
      Op( 500, YFX, "\\/" ),
      Op( 400, YFX, "*" ),
      Op( 400, YFX, "/" ),
      Op( 400, YFX, "//" ),
      Op( 400, YFX, "rem" ),
      Op( 400, YFX, "mod" ),
      Op( 400, YFX, "<<" ),
      Op( 400, YFX, ">>" ),
      Op( 200, XFX, "**" ),
      Op( 200, XFY, "^" ),
      Op( 200, FY, "-" ),
      Op (200, FY, "\\" )), (r, s, x) => StructureAST( r, s, List(x) ), (x, r, s, y) => StructureAST( r, s, List(x, y) ) )

  val expression = rules(1200)

  rule1200.ref = rules(1200)
  rule900.ref = rules(900)
  println( rules )

//  val input = """ (X = 3; X = 4), writeln( X ) """
//  val lexer = new Lexer( ops ++ List("(", ")", ".", "[", "]", "|", "!") )
//  println( "parsing..." )
//  val tokens = lexer.tokenStream( new StringReader(input) )
//  val ast = expression( tokens )
//  println( "done parsing" )
//
//  println( prettyPrint(ast) )

  val lexer = new Lexer( ops ++ List("(", ")", ".", "[", "]", "|", "!") )
  val clauses = new ListBuffer[AST]

  def clause( t: LazyList[Token] ): Unit =
    if (!t.head.isInstanceOf[EOIToken]) {
      expression( t ) match {
        case Success( result, rest ) =>
          clauses += result

          if (rest.head.value == ".")
            clause( rest.tail )
          else
            sys.error( s"expected '.': ${rest.head}" )
      }

    }

  clause( lexer.tokenStream(Reader.fromFile("/home/ed/dev/prolog/examples/sudoku.prolog")) )
  println( clauses.length )

  //  val integers = Rule.oneOrMoreSeparated( Rule.integer, Rule.atomMatch(",") )
  //  val p = new Parser( integers, List(",") )
  //  val input = "345 asdf"
  //
  //  val ast = p( new StringReader(input) )
  //
  //  println( ast )

}