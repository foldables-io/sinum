package io.foldables.sinum

import cats.Show
import cats.implicits.*

enum Value:
  case Atom(value: String) extends Value
  case Cons(value: List[Value])
  case Number(value: Int)
  case Str(value: String)
  case Fun(value: Value.Function)
  case Lambda(value: Value.Function, ctx: EnvCtx)
  case Bool(value: Boolean)
  case Nil

object Value:

  final case class Function(fn: List[Value] => Eval[Value])

  def show(value: Value): String =
    value match
      case Value.Atom(value) => value
      case Value.Cons(value) => value.map(show).mkString_("(", " ", ")")
      case Value.Number(value) => value.toString
      case Value.Str(value) => s"\"$value\""
      case Value.Fun(_) => "(internal function)"
      case Value.Lambda(_, _) => "(lambda function)"
      case Value.Nil => "#nil"
      case Value.Bool(value) => if (value) "#true" else "#false"

  given Show[Value] =
    new Show:
      def show(t: Value): String = 
        Value.show(t)
