package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.Reader


abstract class AST {

  val pos: Reader

}

case class IntegerAST( pos: Reader, n: Int ) extends AST

case class StringAST( pos: Reader, s: String ) extends AST

case class AtomAST( pos: Reader, atom: String ) extends AST

case class VariableAST( pos: Reader, name: String ) extends AST

case class AnonymousAST( pos: Reader ) extends AST

case class StructureAST( pos: Reader, name: String, args: List[AST] ) extends AST

case class ListAST( pos: Reader, args: List[AST], last: Option[AST] ) extends AST
