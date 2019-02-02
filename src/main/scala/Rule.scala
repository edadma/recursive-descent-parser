package xyz.hyperreal.recursive_descent_parser

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import xyz.hyperreal.pattern_matcher.Reader


abstract class Rule[+R] {

  def parse( t: Stream[Token] ): Result[R]

  def apply( t: Stream[Token] ) =  {
//    println( s">>> $this" )
    parse( t )
  }

  def operator( t: Stream[Token], syms: Set[String] ): Result[(Reader, String)] = {
    t.head match {
      case tok@(_: SymbolToken | _: IdentToken) if syms(tok.value) => Success((tok.pos, tok.value), t.tail)
      case _ => Failure( s"expected one of: $syms", t )
    }
  }

}

class LazyRule[R]( r: => Rule[R] ) extends Rule[R] {

  lazy val r1 = r

  def parse( t: Stream[Token] ) = r1( t )

}

class RuleRef[R] extends Rule[R] {

  var ref: Rule[R] = _

  def parse( t: Stream[Token] ) = ref( t )

}

case class Alternates[R]( rs: List[Rule[R]] ) extends Rule[R] {

  require( rs nonEmpty, "there must be at least one alternate" )

  def parse( t: Stream[Token] ): Result[R] = {
    def alternates( alts: List[Rule[R]] ): Result[R] =
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

case class Optional[R]( rule: Rule[R] ) extends Rule[Option[R]] {

  def parse( t: Stream[Token] ): Result[Option[R]] =
    rule( t ) match {
      case Success( result, rest ) => Success(Some(result), rest)
      case _: Failure => Success(None, t)
    }

}

case class Action[R, S]( rule: Rule[R], action: R => S ) extends Rule[S] {

  def parse( t: Stream[Token] ): Result[S] =
    rule( t ) match {
      case Success( result, rest ) => Success(action(result), rest)
      case f: Failure => f
    }

}

case class Sequence[R, S, T]( left: Rule[R], right: Rule[S], action: (R, S) => T ) extends Rule[T] {

  def parse( t: Stream[Token] ) =
    left( t ) match {
      case Success( result, rest ) =>
        right( rest ) match {
          case Success( result1, rest1 ) => Success(action(result, result1), rest1)
          case f: Failure => f
        }
      case f: Failure => f
    }

}

case class SequenceLeft[R, S]( left: Rule[R], right: Rule[S] ) extends Rule[R] {

  def parse( t: Stream[Token] ) =
    left( t ) match {
      case Success( result, rest ) =>
        right( rest ) match {
          case Success( _, rest1 ) => Success(result, rest1)
          case f: Failure => f
        }
      case f: Failure => f
    }

}

case class SequenceRight[R, S]( left: Rule[R], right: Rule[S] ) extends Rule[S] {

  def parse( t: Stream[Token] ) =
    left( t ) match {
      case Success( _, rest ) => right( rest )
      case f: Failure => f
    }

}

case class ZeroOrMore[R]( repeated: Rule[R] ) extends Rule[List[R]] {

  def parse( t: Stream[Token] ) = rep( t )

  def rep( t: Stream[Token], buf: ListBuffer[R] = new ListBuffer ): Result[List[R]] =
    repeated( t ) match {
      case Success( result, rest ) =>
        buf += result
        rep( rest, buf )
      case _ => Success(buf.toList, t)
    }

}

case class LeftAssocInfix[R]( higher: Rule[R], same: Rule[R], ops: Set[String], action: (R, Reader, String, R) => R ) extends Rule[R] {

  val same1 = if (same eq null) this else same

  def parse( t: Stream[Token] ) = {

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

case class RightAssocInfix[R]( higher: Rule[R], same: Rule[R], ops: Set[String], action: (R, Reader, String, R) => R ) extends Rule[R] {

  val same1 = if (same eq null) this else same

  def parse( t: Stream[Token] ): Result[R] =
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

case class NonAssocInfix[R]( higher: Rule[R], fallback: Boolean, ops: Set[String], action: (R, Reader, String, R) => R ) extends Rule[R] {

  def parse( t: Stream[Token] ) =
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

case class AssocPrefix[R]( higher: Rule[R], same: Rule[R], ops: Set[String], action: (Reader, String, R) => R ) extends Rule[R] {

  val same1 = if (same eq null) this else same

  def parse( t: Stream[Token] ) =
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

case class NonAssocPrefix[R]( higher: Rule[R], fallback: Boolean, ops: Set[String], action: (Reader, String, R) => R ) extends Rule[R] {

  def parse( t: Stream[Token] ) =
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

case class Succeed[R]( result: R ) extends Rule[R] {

  def parse( t: Stream[Token] ) = Success(result, t)

}

case class Fail( msg: String ) extends Rule {

  def parse( t: Stream[Token] ) = Failure( msg, t )

}

class TokenClassRule[R]( pred: Token => Boolean, action: (Reader, String) => R, error: String ) extends Rule[R] {

  def parse( t: Stream[Token] ) =
    if (pred( t.head))
      Success(action(t.head.pos, t.head.value), t.tail)
    else
      Failure( error, t )

}

class TokenMatchRule[R]( tok: Class[_], value: String, action: (Reader, String) => R, error: String ) extends Rule[R] {

  def parse( t: Stream[Token] ) =
    if (t.head.getClass == tok && t.head.value == value)
      Success(action(t.head.pos, t.head.value), t.tail)
    else
      Failure( error, t )

}

object Rule {

  def oneOrMoreSeparated[R]( repeated: Rule[R], separator: Rule[_] ) =
    Sequence[R, List[R], List[R]](repeated, ZeroOrMore(SequenceRight(separator, repeated)), _ :: _ )

  def middle[R]( left: Rule[_], middle: Rule[R], right: Rule[_] ) = SequenceLeft( SequenceRight(left, middle), right )

  def atom( s: String ) = new TokenMatchRule( classOf[IdentToken], s, (_, _), s"expected '$s'" )

  def symbol( s: String ) = new TokenMatchRule[(Reader, String)]( classOf[SymbolToken], s, (_, _), s"expected '$s'" )

}
