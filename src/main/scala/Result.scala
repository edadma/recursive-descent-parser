package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.Reader


trait Result

case class Failure( pos: Reader ) extends Result
case class Success( rest: Reader, result: AST ) extends Result