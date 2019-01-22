package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.Reader


abstract class Rule {

  def apply( r: Reader ): Result

  def skipSpace( r: Reader ): Reader =
    if (r.eoi)
      r
    else
      r.ch match {
        case ' '|'\n' => skipSpace( r.next )
        case _ => r
      }

}

class LeftAssocBinary( rule: Rule, ops: Set[String] ) extends Rule {

  def apply( r: Reader ): Result =
    rule( r ) match {

    }

}

class Alternates( rules: List[Rule] ) extends Rule {

  require( rules nonEmpty, "there must be at least one alternate" )

  def apply( r: Reader ): Result = {
    def alternates( rs: List[Rule] ): Result =
      rs match {
        case List( rule ) => rule( r )
        case hd :: tl =>
          hd( r ) match {
            case _: Failure => alternates( tl )
            case s: Success => s
          }
      }

      alternates( rules )
  }

}

class IntegerRule extends Rule {

  def apply( r: Reader ): Result = {
    val res = new StringBuilder
    val r2 = skipSpace( r )

    def parseInt( r1: Reader ): Result = {
      def result: Result =
        if (res.isEmpty)
          Failure( r1 )
        else
          Success( skipSpace(r1), IntegerAST(r2, res.toInt) )

      if (r1.eoi)
        result
      else if (r1.ch.isDigit) {
        res += r1.ch
        parseInt( r1.next )
      } else
        result
    }

    parseInt( r2 )
  }

}
