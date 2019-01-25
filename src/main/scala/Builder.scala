package xyz.hyperreal.recursive_descent_parser


case class Op( priority: Int, specifier: Symbol, operator: String )

object Builder {

  def apply( primary: Rule, grammar: RuleRef, ops: List[Op] ) = {
    var higher = primary

    def rule( cls: Symbol, same: Rule, os: Set[String] ) =
      cls match {
        case 'xfx => NonAssocInfix( higher, os )
        case 'yfx => LeftAssocInfix( higher, same, os )
        case 'xfy => RightAssocInfix( higher, same, os )
      }

    (ops groupBy (_.priority) toList) sortBy (_._1) map {case (p, os) => (p, os groupBy (_.specifier) toList)} foreach {
      case (_, List((cls, os))) => higher = rule( cls, null, os map (_.operator) toSet )
      case (_, cs) =>
        val same = new RuleRef
        val alt = Alternates( cs map {case (cls, os) => rule(cls, same, os map (_.operator) toSet)} )

        same.ref = alt
        higher = alt
    }

    grammar.ref = higher
    (higher, ops.map( _.operator ) ++ List("(", ")"))
  }

}