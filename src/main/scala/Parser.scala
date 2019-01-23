package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.{Matchers, Reader}


class Parser {

  abstract class Token {
    val pos: Reader
    val value: String
  }

  case class IdentToken( pos: Reader, value: String ) extends Token
  case class SymbolToken( pos: Reader, value: String ) extends Token
  case class IntegerToken( pos: Reader, value: String ) extends Token
  case class ErrorToken( pos: Reader, value: String ) extends Token
  case class EOIToken( pos: Reader ) extends Token { val value = null }

  object Lexer extends Matchers[Reader] {

    delimiters += (
      ":-", "-->",
      "?-",
      ";",
      "->",
      ",",
      "\\+",
      "=", "\\=",
      "==", "\\==", "@<", "@=<", "@>", "@>=",
      "=..",
      "=",
      "=:=", "=\\=", "<", "=<", ">", ">=",
      "+", "-", "/\\", "\\/",
      "*", "/", "//", "<<", ">>",
      "**",
      "^",
      "\\",
      ".", "[", "|", "]", "(", ")", "!"
    )

    override val lineComment = '%'

    def token =
      pos <~ eoi ^^ EOIToken |
      pos ~ ident ^^ { case p ~ n => IdentToken( p, n ) } |
      pos ~ delimiter ^^ { case p ~ d => SymbolToken( p, d ) } |
      pos ~ t(digits) ^^ { case p ~ n => IntegerToken( p, n ) } |
      pos ~ char ^^ { case p ~ c => ErrorToken( p, c.toString ) }

  }

  trait Result

  case class Failure( msg: String, rest: Stream[Token] ) extends Result
  case class Success( rest: Stream[Token], result: AST ) extends Result

  def tokenStream( r: Reader ): Stream[Token] = {
    Lexer.token( r ) match {
      case m: Lexer.Mismatch => m.error
      case Lexer.Match( result, next ) => result #:: tokenStream( next )
    }
  }

  def apply( r: Reader, rule: Rule ) = rule( tokenStream(r) )

  abstract class Rule {

    def apply( t: Stream[Token] ): Result

  }

  class Alternates( rs: List[Rule] ) extends Rule {

    require( rs nonEmpty, "there must be at least one alternate" )

    def apply( t: Stream[Token] ): Result = {
      def alternates( alts: List[Rule] ): Result =
        alts match {
          case List( r ) => r( t )
          case hd :: tl =>
            hd( t ) match {
              case _: Failure => alternates( tl )
              case s: Success => s
            }
        }

      alternates( rs )
    }

  }

  object IntegerRule extends Rule {

    def apply( t: Stream[Token] ) =
      t.head match {
        case IntegerToken( pos, value ) => Success( t.tail, IntegerAST(pos, value.toInt) )
        case _ => Failure( "expected integer", t )
      }
  }

//class LeftAssocBinary( p: Parser, ops: Set[String] ) extends Parser {
//
//  def apply( r: Reader ): Result =
//
//  def expr( r: Result, left: AST ): Result =
//    p( r ) match {
//      case f: Failure => f
//      case s: Success =>
//
//    }
//
//
//}


}
