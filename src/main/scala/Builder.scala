package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.mutable


case class Op( priority: Int, specifier: Symbol, operator: String )

object Builder {

  def apply[R]( primary: Rule[R], ops: List[Op], unaryAction: (Reader, String, R) => R, binaryAction: (R, Reader, String, R) => R ) = {
    var higher = primary
    val ruleMap = new mutable.HashMap[Int, Rule[R]]

    def rule( cls: Symbol, same: Rule[R], fallback: Boolean, os: Set[String] ) =
      cls match {
        case 'xfx => NonAssocInfix( higher, fallback, os, binaryAction )
        case 'yfx => LeftAssocInfix( higher, same, os, binaryAction )
        case 'xfy => RightAssocInfix( higher, same, os, binaryAction )
        case 'fx => NonAssocPrefix( higher, fallback, os, unaryAction )
        case 'fy => AssocPrefix( higher, same, os, unaryAction )
      }

    (ops groupBy (_.priority) toList) sortBy (_._1) map {case (p, os) => (p, os groupBy (_.specifier) toList)} foreach {
      case (p, List((cls, os))) =>
        higher = rule( cls, null, true, os map (_.operator) toSet )
        ruleMap(p) = higher
      case (p, cs) =>
        val same = new RuleRef[R]
        val alt = Alternates( (cs map {case (cls, os) => rule(cls, same, false, os map (_.operator) toSet)}) :+ higher )

        same.ref = alt
        higher = alt
        ruleMap(p) = higher
    }

    (ruleMap toMap, ops.map( _.operator ) ++ List("(", ")"))
  }

}