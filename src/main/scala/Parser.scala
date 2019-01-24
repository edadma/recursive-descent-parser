package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.{Matchers, Reader}

import scala.collection.mutable.ArrayBuffer


abstract class Token {
  val pos: Reader
  val value: String
}

case class IdentToken( pos: Reader, value: String ) extends Token
case class SymbolToken( pos: Reader, value: String ) extends Token
case class IntegerToken( pos: Reader, value: String ) extends Token
case class ErrorToken( pos: Reader, value: String ) extends Token
case class EOIToken( pos: Reader ) extends Token { val value = null }

trait Result

case class Failure( msg: String, rest: Stream[Token] ) extends Result
case class Success( rest: Stream[Token], result: AST ) extends Result

class Parser( grammar: Rule ) {

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

  def tokenStream( r: Reader ): Stream[Token] = {
    Lexer.token( r ) match {
      case m: Lexer.Mismatch => m.error
      case Lexer.Match( result, next ) => result #:: tokenStream( next )
    }
  }

  def apply( r: Reader ) = grammar( tokenStream(r) )

}

abstract class Rule {

  def apply( t: Stream[Token] ): Result

  def symbol( t: Stream[Token], syms: Set[String] ) = {
    t.head match {
      case SymbolToken( pos, value ) if syms(value) => Success( t.tail, StringAST(pos, value) )
      case tok => Failure( s"expected one of: $syms", t )
    }
  }

}

class LazyRule( r: => Rule ) extends Rule {

  lazy val r1 = r

  def apply( t: Stream[Token] ) = r1( t )

}

class RuleRef extends Rule {

  var r: Rule = null

  def apply( t: Stream[Token] ) = r( t )

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

class Sequence( rs: List[Rule], action: Vector[AST] => AST ) extends Rule {

  val len = rs.length

  def apply( t: Stream[Token] ): Result = {
    val results = new ArrayBuffer[AST]

    def rules( l: List[Rule], t1: Stream[Token] ): Result =
      l match {
        case Nil => Success( t1, action(results.toVector) )
        case hd :: tl =>
          hd( t1 ) match {
            case f: Failure => f
            case Success( rest, result ) =>
              results += result
              rules( tl, rest )
          }
      }

    rules( rs, t )
  }

}

class LeftAssocInfix( expr: Rule, ops: Set[String] ) extends Rule {

  def apply( t: Stream[Token] ) = {

    def parse_expr( suc: Success ): Result = {
      symbol( suc.rest, ops ) match {
        case Success( rest, StringAST(pos, value) ) =>
          expr( rest ) match {
            case _: Failure => suc
            case Success( rest1, right ) => parse_expr( Success(rest1, BinaryAST(suc.result, pos, value, right)) )
          }
        case _ => suc
      }
    }

    expr( t ) match {
      case f: Failure => f
      case s: Success => parse_expr( s )
    }

  }

}

class RightAssocInfix( expr: Rule, ops: Set[String] ) extends Rule {

  def apply( t: Stream[Token] ) =
    expr( t ) match {
      case f: Failure => f
      case suc@Success( rest, result ) =>
        symbol( rest, ops ) match {
          case Success( rest1, StringAST(pos, s) ) =>
            apply( rest1 ) match {
              case Success( rest2, result1 ) => Success( rest2, BinaryAST(result, pos, s, result1) )
              case _ => suc
            }
          case _ => suc
        }
    }

}

class NonAssocInfix( expr: Rule, ops: Set[String] ) extends Rule {

  def apply( t: Stream[Token] ) =
    expr( t ) match {
      case f: Failure => f
      case suc@Success( rest, result ) =>
        symbol( rest, ops ) match {
          case Success( rest1, StringAST(pos, s) ) =>
            expr( rest1 ) match {
              case Success( rest2, result1 ) => Success( rest2, BinaryAST(result, pos, s, result1) )
              case _ => suc
            }
          case _ => suc
        }
    }

}

class Succeed( result: AST ) extends Rule {

  def apply( t: Stream[Token] ) = Success( t, result )

}

class Fail( msg: String ) extends Rule {

  def apply( t: Stream[Token] ) = Failure( msg, t )

}

class TokenClassRule( tok: Class[_], action: (Reader, String) => AST, error: String ) extends Rule {

  def apply( t: Stream[Token] ) =
    if (t.head.getClass == tok)
      Success( t.tail, action(t.head.pos, t.head.value) )
    else
      Failure( error, t )

}

class TokenMatchRule( tok: Class[_], value: String, action: (Reader, String) => AST, error: String ) extends Rule {

  def apply( t: Stream[Token] ) =
    if (t.head.getClass == tok && t.head.value == value)
      Success( t.tail, action(t.head.pos, t.head.value) )
    else
      Failure( error, t )

}

object IntegerRule extends TokenClassRule( classOf[IntegerToken], (r, s) => IntegerAST(r, s.toInt), "expected integer" )

object LeftParenRule extends TokenMatchRule( classOf[SymbolToken], "(", (_, _) => null, "expected '('" )

object RightParenRule extends TokenMatchRule( classOf[SymbolToken], ")", (_, _) => null, "expected ')'" )
