package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.Reader


trait Result

case class Failure( msg: String, pos: Reader ) extends Result
case class Success( rest: Stream[Token], result: AST ) extends Result