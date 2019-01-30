package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.{Matchers, Reader}


abstract class Token {
  val pos: Reader
  val value: String
}

case class StringToken( pos: Reader, value: String ) extends Token
case class AtomToken( pos: Reader, value: String ) extends Token
case class QuotedAtomToken( pos: Reader, value: String ) extends Token
case class SymbolToken( pos: Reader, value: String ) extends Token
case class IntegerToken( pos: Reader, value: String ) extends Token
case class ErrorToken( pos: Reader, value: String ) extends Token
case class EOIToken( pos: Reader ) extends Token { val value = null }

abstract class Result[+R]
case class Failure( msg: String, rest: Stream[Token] ) extends Result
case class Success[R]( rest: Stream[Token], result: R ) extends Result[R]

class Parser[R]( val grammar: Rule[R], delims: List[String] ) {

  object Lexer extends Matchers[Reader] {

    delimiters ++= delims

    override val lineComment = '%'

    def token =
      pos <~ eoi ^^ EOIToken |
      pos ~ singleStringLit ^^ { case p ~ n => QuotedAtomToken( p, n ) } |
      pos ~ ident ^^ { case p ~ n => AtomToken( p, n ) } |
      pos ~ delimiter ^^ { case p ~ d => SymbolToken( p, d ) } |
      pos ~ t(digits) ^^ { case p ~ n => IntegerToken( p, n ) } |
      pos ~ char ^^ { case p ~ c => ErrorToken( p, c.toString ) }

  }

  def tokenStream( r: Reader ): Stream[Token] = {
    Lexer.token( r ) match {
      case m: Lexer.Mismatch => m.error
      case Lexer.Match( result, next ) => result #:: tokenStream( next )
    }
  }

  def apply( r: Reader ) = grammar( tokenStream(r) )

}
