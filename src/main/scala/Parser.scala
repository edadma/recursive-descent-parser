package xyz.hyperreal.recursive_descent_parser

import scala.collection.mutable.ListBuffer
import xyz.hyperreal.pattern_matcher.Reader


abstract class Parser[+R] {

  def parse( t: LazyList[Token] ): Result[R]

  def apply( t: LazyList[Token] ) =  {
//    println( s">>> $this" )
    parse( t )
  }

  def operator( t: LazyList[Token], syms: Set[String] ): Result[(Reader, String)] = {
    t.head match {
      case tok@(_: SymbolToken | _: IdentToken) if syms(tok.value) => Success((tok.pos, tok.value), t.tail)
      case _ => Failure( s"expected one of: $syms", t )
    }
  }

}

class LazyParser[R](r: => Parser[R] ) extends Parser[R] {

  lazy val r1 = r

  def parse( t: LazyList[Token] ) = r1( t )

}

class ParserRef[R] extends Parser[R] {

  var ref: Parser[R] = _

  def parse( t: LazyList[Token] ) = ref( t )

}

case class Alternates[R]( rs: List[Parser[R]] ) extends Parser[R] {

  require( rs nonEmpty, "there must be at least one alternate" )

  def parse( t: LazyList[Token] ): Result[R] = {
    def alternates( alts: List[Parser[R]] ): Result[R] =
      alts match {
        case List( r ) => r( t )
        case hd :: tl =>
          hd( t ) match {
            case _: Failure => alternates( tl )
            case s: Success[R] => s
          }
      }

    alternates( rs )
  }

}

case class Optional[R]( rule: Parser[R] ) extends Parser[Option[R]] {

  def parse( t: LazyList[Token] ): Result[Option[R]] =
    rule( t ) match {
      case Success( result, rest ) => Success(Some(result), rest)
      case _: Failure => Success(None, t)
    }

}

case class Action[R, S](rule: Parser[R], action: R => S ) extends Parser[S] {

  def parse( t: LazyList[Token] ): Result[S] =
    rule( t ) match {
      case Success( result, rest ) => Success(action(result), rest)
      case f: Failure => f
    }

}

case class Sequence[R, S, T](left: Parser[R], right: Parser[S], action: (R, S) => T ) extends Parser[T] {

  def parse( t: LazyList[Token] ) =
    left( t ) match {
      case Success( result, rest ) =>
        right( rest ) match {
          case Success( result1, rest1 ) => Success(action(result, result1), rest1)
          case f: Failure => f
        }
      case f: Failure => f
    }

}

case class SequenceLeft[R, S](left: Parser[R], right: Parser[S] ) extends Parser[R] {

  def parse( t: LazyList[Token] ) =
    left( t ) match {
      case Success( result, rest ) =>
        right( rest ) match {
          case Success( _, rest1 ) => Success(result, rest1)
          case f: Failure => f
        }
      case f: Failure => f
    }

}

case class SequenceRight[R, S](left: Parser[R], right: Parser[S] ) extends Parser[S] {

  def parse( t: LazyList[Token] ) =
    left( t ) match {
      case Success( _, rest ) => right( rest )
      case f: Failure => f
    }

}

case class ZeroOrMore[R]( repeated: Parser[R] ) extends Parser[List[R]] {

  def parse( t: LazyList[Token] ) = rep( t )

  def rep( t: LazyList[Token], buf: ListBuffer[R] = new ListBuffer ): Result[List[R]] =
    repeated( t ) match {
      case Success( result, rest ) =>
        buf += result
        rep( rest, buf )
      case _ => Success(buf.toList, t)
    }

}

case class LeftAssocInfix[R](higher: Parser[R], same: Parser[R], ops: Set[String], action: (R, Reader, String, R) => R ) extends Parser[R] {

  val same1 = if (same eq null) this else same

  def parse( t: LazyList[Token] ) = {

    def parse_expr( suc: Success[R] ): Result[R] = {
      operator( suc.rest, ops ) match {
        case Success( (pos, value), rest ) =>
          higher( rest ) match {
            case _: Failure => suc
            case Success( right, rest1 ) => parse_expr( Success(action(suc.result, pos, value, right), rest1) )
          }
        case _ => suc
      }
    }

    higher( t ) match {
      case f: Failure => f
      case s: Success[R] => parse_expr( s )
    }

  }

}

case class RightAssocInfix[R](higher: Parser[R], same: Parser[R], ops: Set[String], action: (R, Reader, String, R) => R ) extends Parser[R] {

  val same1 = if (same eq null) this else same

  def parse( t: LazyList[Token] ): Result[R] =
    higher( t ) match {
      case suc@Success( result, rest ) =>
        operator( rest, ops ) match {
          case Success( (pos, s), rest1 ) =>
            same1( rest1 ) match {
              case Success( result1, rest2 ) => Success(action(result, pos, s, result1), rest2)
              case _ if same eq null => suc
              case f: Failure => f
            }
          case _ if same eq null => suc
          case f: Failure => f
        }
      case f => f
    }

}

case class NonAssocInfix[R](higher: Parser[R], fallback: Boolean, ops: Set[String], action: (R, Reader, String, R) => R ) extends Parser[R] {

  def parse( t: LazyList[Token] ) =
    higher( t ) match {
      case f: Failure => f
      case suc@Success( result, rest ) =>
        operator( rest, ops ) match {
          case Success( (pos, s), rest1 ) =>
            higher( rest1 ) match {
              case Success( result1, rest2 ) => Success(action(result, pos, s, result1), rest2)
              case _ if fallback => suc
              case f: Failure => f
            }
          case _ if fallback => suc
          case f: Failure => f
        }
    }

}

case class AssocPrefix[R](higher: Parser[R], same: Parser[R], ops: Set[String], action: (Reader, String, R) => R ) extends Parser[R] {

  val same1 = if (same eq null) this else same

  def parse( t: LazyList[Token] ) =
    operator( t, ops ) match {
      case Success( (pos, s), rest ) =>
        same1( rest ) match {
          case Success( result, rest1 ) => Success(action( pos, s, result), rest1)
          case _ => higher( t )
        }
      case _ if same eq null => higher( t )
      case f: Failure => f
    }

}

case class NonAssocPrefix[R](higher: Parser[R], fallback: Boolean, ops: Set[String], action: (Reader, String, R) => R ) extends Parser[R] {

  def parse( t: LazyList[Token] ) =
    operator( t, ops ) match {
      case Success( (pos, s), rest ) =>
        higher( rest ) match {
          case Success( result, rest1 ) => Success(action(pos, s, result), rest1)
          case _ if fallback => higher( t )
          case f: Failure => f
        }
      case _ if fallback => higher( t )
      case f: Failure => f
    }

}

case class Succeed[R]( result: R ) extends Parser[R] {

  def parse( t: LazyList[Token] ) = Success(result, t)

}

case class Fail( msg: String ) extends Parser {

  def parse( t: LazyList[Token] ) = Failure( msg, t )

}

class TokenClassParser[R](pred: Token => Boolean, action: (Reader, String) => R, error: String ) extends Parser[R] {

  def parse( t: LazyList[Token] ) =
    if (pred( t.head))
      Success(action(t.head.pos, t.head.value), t.tail)
    else
      Failure( error, t )

}

class TokenMatchParser[R](tok: Class[_], value: String, action: (Reader, String) => R, error: String ) extends Parser[R] {

  def parse( t: LazyList[Token] ) =
    if (t.head.getClass == tok && t.head.value == value)
      Success(action(t.head.pos, t.head.value), t.tail)
    else
      Failure( error, t )

}

object Parser {

  def oneOrMoreSeparated[R](repeated: Parser[R], separator: Parser[_] ) =
    Sequence[R, List[R], List[R]](repeated, ZeroOrMore(SequenceRight(separator, repeated)), _ :: _ )

  def middle[R](left: Parser[_], middle: Parser[R], right: Parser[_] ) = SequenceLeft( SequenceRight(left, middle), right )

  def atom( s: String ) = new TokenMatchParser( classOf[IdentToken], s, (_, _), s"expected '$s'" )

  def symbol( s: String ) = new TokenMatchParser[(Reader, String)]( classOf[SymbolToken], s, (_, _), s"expected '$s'" )

}
