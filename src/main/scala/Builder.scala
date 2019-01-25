package xyz.hyperreal.recursive_descent_parser


case class Op( priority: Int, specifier: Symbol, operator: String )

object Builder {

  def apply( primary: Rule, grammar: RuleRef, ops: List[Op] ) = {
    var higher = primary

    (ops groupBy (_.priority) toList) sortBy (_._1) map {case (p, os) => (p, os groupBy (_.specifier) toList)} foreach {
      case (_, List(cls)) =>
        cls match {
          case ('yfx, os) => higher = LeftAssocInfix( higher, null, os map (_.operator) toSet )
        }
    }

    grammar.r = higher
    higher
  }

}