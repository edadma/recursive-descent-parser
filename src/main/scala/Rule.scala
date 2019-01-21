package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.Reader


abstract class Rule {

  def apply( r: Reader, p: Parser ): Result

  def skipSpace( r: Reader ): Reader =
    if (r.eoi)
      r
    else
      r.ch match {
        case ' '|'\n' => skipSpace( r.next )
        case _ => r
      }

}

//class LeftAssocBinary extends Rule {
//
//  def apply( r: Reader ): (Reader, AST)
//
//}

class Alternates( rules: List[Rule] ) extends Rule {

  def apply( r: Reader, p: Parser ): Result = {
    def alternates( rs: List[Rule] ) =
      rs match {
        case Nil => Left( r )
        case hd :: tl =>
      }


  }

}

class IntegerRule extends Rule {

  def apply( r: Reader, p: Parser ): Result = {
    val res = new StringBuilder
    val r2 = skipSpace( r )

    def parseInt( r1: Reader ): Result = {
      def result: Result =
        if (res.isEmpty)
          Failure( r1 )
        else
          Success( r1, IntegerAST(r2, res.toInt) )

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

class Primary extends Rule {

  def apply( r: Reader, p: Parser ) = {
    val r1 = skipSpace( r )

  }

}