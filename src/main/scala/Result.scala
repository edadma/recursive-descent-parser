package xyz.hyperreal.recursive_descent_parser


abstract class Result[+R]

case class Failure( msg: String, rest: LazyList[Token] ) extends Result {
  def error = rest.head.pos.error( msg )
}

case class Success[R]( result: R, rest: LazyList[Token] ) extends Result[R]