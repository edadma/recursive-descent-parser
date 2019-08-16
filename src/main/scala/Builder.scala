package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.mutable


abstract class Assoc
case object XFX extends Assoc
case object YFX extends Assoc
case object XFY extends Assoc
case object FX extends Assoc
case object FY extends Assoc

case class Op( priority: Int, specifier: Assoc, operator: String )


object Builder {

  def apply[R]( primary: Parser[R], ops: List[Op], unaryAction: (Reader, String, R) => R, binaryAction: (R, Reader, String, R) => R ) = {
    var higher = primary
    val ruleMap = new mutable.HashMap[Int, Parser[R]]

    def rule( cls: Assoc, same: Parser[R], fallback: Boolean, os: Set[String] ) =
      cls match {
        case XFX => NonAssocInfix( higher, fallback, os, binaryAction )
        case YFX => LeftAssocInfix( higher, same, os, binaryAction )
        case XFY => RightAssocInfix( higher, same, os, binaryAction )
        case FX => NonAssocPrefix( higher, fallback, os, unaryAction )
        case FY => AssocPrefix( higher, same, os, unaryAction )
      }

    (ops groupBy (_.priority) toList) sortBy (_._1) map {case (p, os) => (p, os groupBy (_.specifier) toList)} foreach {
      case (p, List((cls, os))) =>
        higher = rule( cls, null, true, os map (_.operator) toSet )
        ruleMap(p) = higher
      case (p, cs) =>
        val same = new ParserRef[R]
        val alt = Alternates( (cs map {case (cls, os) => rule(cls, same, false, os map (_.operator) toSet)}) :+ higher )

        same.ref = alt
        higher = alt
        ruleMap(p) = higher
    }

    (ruleMap toMap, ops.map( _.operator ) ++ List("(", ")"))
  }

}