package io.foldables.sinum

import cats.implicits.*
import cats.parse.{Parser => P, Parser0 => P0}

object Parser:
  val str: P[Value] =
    for {
      _ <- P.char('"')
      s <- P.charsWhile(c => c != '"')
      _ <- P.char('"')
    } yield Value.Str(s)

  val atom: P[Value] =
    P.charsWhile(_.isLetterOrDigit).map(Value.Atom.apply)

  val number: P[Value] =
    P.charsWhile(_.isDigit).map(_.toInt).map(Value.Number.apply)

  val reserved: P[Value] =
    P.string("#true").as(Value.Bool(true)).orElse(P.string("#false").as(Value.Bool(false))).orElse(P.string("#nil").as(Value.Nil))

  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val whitespaces0: P0[Unit] = whitespace.rep0.void

  def expression: P[Value] = P.recursive[Value] { recurse =>

    val sExpr: P[Value] =
      recurse.repSep0(whitespaces0).with1.between(P.char('('), P.char(')')).map(Value.Cons.apply)

    str.orElse(number).orElse(reserved).orElse(atom).orElse(sExpr).surroundedBy(whitespaces0)
  }
  
  def parse(in: String): Either[Error, Value] =
    expression.parseAll(in).leftMap(e => Error.ParseError(e.toString))

