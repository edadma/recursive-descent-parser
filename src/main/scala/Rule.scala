package xyz.hyperreal.recursive_descent_parser

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import xyz.hyperreal.pattern_matcher.Reader


abstract class Rule[+R] {

  def apply( t: Stream[Token] ): Result[R]

  def operator( t: Stream[Token], syms: Set[String] ): Result[(Reader, String)] = {
    t.head match {
      case SymbolToken( pos, value ) if syms(value) => Success( t.tail, (pos, value) )
      case _ => Failure( s"expected one of: $syms", t )
    }
  }

}

class LazyRule[R]( r: => Rule[R] ) extends Rule[R] {

  lazy val r1 = r

  def apply( t: Stream[Token] ) = r1( t )

}

class RuleRef[R] extends Rule[R] {

  var ref: Rule[R] = _

  def apply( t: Stream[Token] ) = ref( t )

}

case class Alternates[R]( rs: List[Rule[R]] ) extends Rule[R] {

  require( rs nonEmpty, "there must be at least one alternate" )

  def apply( t: Stream[Token] ): Result[R] = {
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

  def apply( t: Stream[Token] ): Result[Option[R]] =
    rule( t ) match {
      case Success( rest, result ) => Success( rest, Some(result) )
      case _: Failure => Success( t, None )
    }

}

case class Action[R, S]( rule: Rule[R], action: R => S ) extends Rule[S] {

  def apply( t: Stream[Token] ): Result[S] =
    rule( t ) match {
      case Success( rest, result ) => Success( rest, action(result) )
      case f: Failure => f
    }

}

case class Sequence[R, S, T]( left: Rule[R], right: Rule[S], action: (R, S) => T ) extends Rule[T] {

  def apply( t: Stream[Token] ) =
    left( t ) match {
      case Success( rest, result ) =>
        right( rest ) match {
          case Success( rest1, result1 ) => Success( rest1, action(result, result1) )
          case f: Failure => f
        }
      case f: Failure => f
    }

}

case class SequenceLeft[R, S]( left: Rule[R], right: Rule[S] ) extends Rule[R] {

  def apply( t: Stream[Token] ) =
    left( t ) match {
      case Success( rest, result ) =>
        right( rest ) match {
          case Success( rest1, _ ) => Success( rest1, result )
          case f: Failure => f
        }
      case f: Failure => f
    }

}

case class SequenceRight[R, S]( left: Rule[R], right: Rule[S] ) extends Rule[S] {

  def apply( t: Stream[Token] ) =
    left( t ) match {
      case Success( rest, _ ) => right( rest )
      case f: Failure => f
    }

}

case class ZeroOrMore[R]( repeated: Rule[R] ) extends Rule[List[R]] {

  def apply( t: Stream[Token] ) = rep( t )

  def rep( t: Stream[Token], buf: ListBuffer[R] = new ListBuffer ): Result[List[R]] =
    repeated( t ) match {
      case Success( rest, result ) =>
        buf += result
        rep( rest, buf )
      case _ => Success( t, buf.toList )
    }

}

case class LeftAssocInfix[R]( higher: Rule[R], same: Rule[R], ops: Set[String], action: (R, Reader, String, R) => R ) extends Rule[R] {

  val same1 = if (same eq null) this else same

  def apply( t: Stream[Token] ) = {

    def parse_expr( suc: Success[R] ): Result[R] = {
      operator( suc.rest, ops ) match {
        case Success( rest, (pos, value) ) =>
          higher( rest ) match {
            case _: Failure => suc
            case Success( rest1, right ) => parse_expr( Success(rest1, action(suc.result, pos, value, right)/*BinaryAST(suc.result, pos, value, right)*/) )
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

  def apply( t: Stream[Token] ): Result[R] =
    higher( t ) match {
      case suc@Success( rest, result ) =>
        operator( rest, ops ) match {
          case Success( rest1, (pos, s) ) =>
            same1( rest1 ) match {
              case Success( rest2, result1 ) => Success( rest2, action(result, pos, s, result1) )
              case _ => suc
            }
          case _ if same eq null => suc
          case f => f.asInstanceOf[Result[R]]
        }
      case f => f
    }

}

case class NonAssocInfix[R]( higher: Rule[R], ops: Set[String], action: (R, Reader, String, R) => R ) extends Rule[R] {

  def apply( t: Stream[Token] ) =
    higher( t ) match {
      case f: Failure => f
      case suc@Success( rest, result ) =>
        operator( rest, ops ) match {
          case Success( rest1, (pos, s) ) =>
            higher( rest1 ) match {
              case Success( rest2, result1 ) => Success( rest2, action(result, pos, s, result1) )
              case _ => suc
            }
          case _ => suc
        }
    }

}

case class AssocPrefix[R]( higher: Rule[R], same: Rule[R], ops: Set[String], action: (Reader, String, R) => R ) extends Rule[R] {

  val same1 = if (same eq null) this else same

  def apply( t: Stream[Token] ) =
    operator( t, ops ) match {
      case Success( rest, (pos, s) ) =>
        same1( rest ) match {
          case Success( rest1, result ) => Success( rest1, action( pos, s, result) )
          case _ => higher( t )
        }
      case _ if same eq null => higher( t )
      case f => f.asInstanceOf[Result[R]]
    }

}

case class NonAssocPrefix[R]( higher: Rule[R], ops: Set[String], action: (Reader, String, R) => R ) extends Rule[R] {

  def apply( t: Stream[Token] ) =
    operator( t, ops ) match {
      case Success( rest, (pos, s) ) =>
        higher( rest ) match {
          case Success( rest1, result ) => Success( rest1, action( pos, s, result) )
          case _ => higher( t )
        }
      case _ => higher( t )
    }

}

case class Succeed[R]( result: R ) extends Rule[R] {

  def apply( t: Stream[Token] ) = Success( t, result )

}

case class Fail( msg: String ) extends Rule {

  def apply( t: Stream[Token] ) = Failure( msg, t )

}

class TokenClassRule[R]( tok: Class[_], action: (Reader, String) => R, error: String ) extends Rule[R] {

  def apply( t: Stream[Token] ) =
    if (t.head.getClass == tok)
      Success( t.tail, action(t.head.pos, t.head.value) )
    else
      Failure( error, t )

}

class TokenMatchRule[R]( tok: Class[_], value: String, action: (Reader, String) => R, error: String ) extends Rule[R] {

  def apply( t: Stream[Token] ) =
    if (t.head.getClass == tok && t.head.value == value)
      Success( t.tail, action(t.head.pos, t.head.value) )
    else
      Failure( error, t )

}

object Rule {

  def oneOrMoreSeparated[R]( repeated: Rule[R], separator: Rule[_] ) =
    Sequence[R, List[R], List[R]](repeated, ZeroOrMore(SequenceRight(separator, repeated)), _ :: _ )

  def middle[R]( left: Rule[_], middle: Rule[R], right: Rule[_] ) = SequenceLeft( SequenceRight(left, middle), right )

  def atom( s: String ) = new TokenMatchRule( classOf[AtomToken], s, (_, _), s"expected '$s'" )

  def symbol( s: String ) = new TokenMatchRule( classOf[SymbolToken], s, (_, _), s"expected '$s'" )

}
