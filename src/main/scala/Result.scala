package xyz.hyperreal.recursive_descent_parser


abstract class Result[+R]
case class Failure( msg: String, rest: Stream[Token] ) extends Result {
  def error = sys.error( msg )
}
case class Success[R]( result: R, rest: Stream[Token] ) extends Result[R]
