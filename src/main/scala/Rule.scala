package xyz.hyperreal.recursive_descent_parser

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import xyz.hyperreal.pattern_matcher.Reader


abstract class Rule {

  def apply( t: Stream[Token] ): Result

  def operator( t: Stream[Token], syms: Set[String] ) = {
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

  var ref: Rule = null

  def apply( t: Stream[Token] ) = ref( t )

}

case class Alternates( rs: List[Rule] ) extends Rule {

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

case class Sequence( rs: List[Rule], action: Vector[AST] => AST ) extends Rule {

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

case class ZeroOrMore( repeated: Rule ) extends Rule {

  def apply( t: Stream[Token] ) = rep( t )

  def rep( t: Stream[Token], buf: ListBuffer[AST] = new ListBuffer ): Result =
    repeated( t ) match {
      case Success( rest, result ) =>
        buf += result
        rep( rest, buf )
      case _ => Success( t, ListAST(null, buf.toList) )
    }

}

case class LeftAssocInfix( higher: Rule, same: Rule, ops: Set[String] ) extends Rule {

  val same1 = if (same eq null) this else same

  def apply( t: Stream[Token] ) = {

    def parse_expr( suc: Success ): Result = {
      operator( suc.rest, ops ) match {
        case Success( rest, StringAST(pos, value) ) =>
          higher( rest ) match {
            case _: Failure => suc
            case Success( rest1, right ) => parse_expr( Success(rest1, BinaryAST(suc.result, pos, value, right)) )
          }
        case _ => suc
      }
    }

    higher( t ) match {
      case f: Failure => f
      case s: Success => parse_expr( s )
    }

  }

}

case class RightAssocInfix( higher: Rule, same: Rule, ops: Set[String] ) extends Rule {

  val same1 = if (same eq null) this else same

  def apply( t: Stream[Token] ) =
    higher( t ) match {
      case suc@Success( rest, result ) =>
        operator( rest, ops ) match {
          case Success( rest1, StringAST(pos, s) ) =>
            same1( rest1 ) match {
              case Success( rest2, result1 ) => Success( rest2, BinaryAST(result, pos, s, result1) )
              case _ => suc
            }
          case _ if same eq null => suc
          case f => f
        }
      case f => f
    }

}

case class NonAssocInfix( higher: Rule, ops: Set[String] ) extends Rule {

  def apply( t: Stream[Token] ) =
    higher( t ) match {
      case f: Failure => f
      case suc@Success( rest, result ) =>
        operator( rest, ops ) match {
          case Success( rest1, StringAST(pos, s) ) =>
            higher( rest1 ) match {
              case Success( rest2, result1 ) => Success( rest2, BinaryAST(result, pos, s, result1) )
              case _ => suc
            }
          case _ => suc
        }
    }

}

case class AssocPrefix( higher: Rule, same: Rule, ops: Set[String] ) extends Rule {

  val same1 = if (same eq null) this else same

  def apply( t: Stream[Token] ) =
    operator( t, ops ) match {
      case Success( rest, StringAST(pos, s) ) =>
        same1( rest ) match {
          case Success( rest1, result ) => Success( rest1, UnaryAST( pos, s, result) )
          case _ => higher( t )
        }
      case _ if same eq null => higher( t )
      case f => f
    }

}

case class NonAssocPrefix( higher: Rule, ops: Set[String] ) extends Rule {

  def apply( t: Stream[Token] ) =
    operator( t, ops ) match {
      case Success( rest, StringAST(pos, s) ) =>
        higher( rest ) match {
          case Success( rest1, result ) => Success( rest1, UnaryAST( pos, s, result) )
          case _ => higher( t )
        }
      case _ => higher( t )
    }

}

case class Succeed( result: AST ) extends Rule {

  def apply( t: Stream[Token] ) = Success( t, result )

}

case class Fail( msg: String ) extends Rule {

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

object Rule {

  def oneOrMoreSeparated( repeated: Rule, separator: Rule ) =
    Sequence( List(
      repeated, ZeroOrMore(Sequence( List(
        separator, repeated), _(1))
      )), v => ListAST(null, v(0) +: v(1).asInstanceOf[ListAST].list) )

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

  def atom( s: String ) = new TokenMatchRule( classOf[AtomToken], s, (_, _) => null, s"expected '$s'" )

  def symbol( s: String ) = new TokenMatchRule( classOf[SymbolToken], s, (_, _) => null, s"expected '$s'" )

}
