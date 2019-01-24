package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.Reader


abstract class AST {

  val pos: Reader

}

case class UnaryAST( pos: Reader, op: String, right: AST ) extends AST

case class BinaryAST( left: AST, pos: Reader, op: String, right: AST ) extends AST

case class IntegerAST( pos: Reader, n: Int ) extends AST

case class StringAST( pos: Reader, s: String ) extends AST
