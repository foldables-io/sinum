package io.foldables.sinum

import cats.Show
import cats.data.ReaderT
import cats.implicits.*
import cats.effect.IO


type EnvCtx = Map[String, Value]

type Eval[A] = ReaderT[IO, EnvCtx, A]

object Eval:

  def pure[A](a: A): Eval[A] =
    ReaderT.pure[IO, EnvCtx, A](a)

  def raise[A](error: Error): Eval[A] =
    ReaderT.liftF(IO.raiseError[A](error))

  def fromParsed(output: Either[Error, Value]): Eval[Value] =
    ReaderT.liftF[IO, EnvCtx, Value](IO.fromEither(output))

  val BasicEnv = Map(
    "read" -> Value.Fun(Value.Function(args => args match
      case List(Value.Str(str)) => Parser.parse(str).fold(_ => Eval.raise(Error.RuntimeError("Invalid Expr")), Eval.pure)
      case other => Eval.raise(Error.ArgumentMismatch("read", 1, other.length))
    )),
  
    "sum" -> Value.Fun(Value.Function(args => (args.foldLeft(Value.Number(0).some) { 
      case (Some(Value.Number(acc)), Value.Number(cur)) => Some(Value.Number(acc + cur))
      case _ => None
    }).fold(Eval.raise(Error.RuntimeError(s"Cannot sum ${(Value.Cons(args): Value).show}")))(Eval.pure)))
  )


  def run(value: Value): Eval[Value] =
    import Value.*
    value match
      case Cons(List(Atom("eval"), value)) => run(value)
      // quote
      case Cons(List(Atom("quote"), value)) => Eval.pure(value)
      // autoquote
      case q @ Number(_) => Eval.pure(q)
      case q @ Str(_) => Eval.pure(q)
      case q @ Bool(_) => Eval.pure(q)
      case q @ Nil => Eval.pure(q)
      case Cons(List()) => Eval.pure(Nil)
      // write
      case Cons(List(Atom("write"), value)) => Eval.pure(Str(value.show))
      case Cons(Atom("write") :: value) => Eval.pure(Str((Cons(value): Value).show))
      // atom
      case Atom(name) => getVar(name)
      // if
      case Cons(Atom("if") :: pred :: t :: f :: List()) => evalIf(pred, t, f)
      // let
      case Cons(Atom("let") :: Cons(pairs) :: expr) => evalLet(pairs, Cons(expr))
      // begin
      case Cons(Atom("begin") :: rest :: List()) => evalBody(rest)
      case Cons(Atom("begin") :: rest) => evalBody(Cons(rest))
      // fold single-element list
      case Cons(List(value)) => run(value)
      // functions
      case Cons(List(Atom("lambda"), Cons(params), expr)) if params.forall(_.isInstanceOf[Atom]) =>
        ReaderT.ask[IO, EnvCtx].map(env => Lambda(Function(applyLambda(expr, params.asInstanceOf[List[Atom]])), env))
      case Cons(Atom("lambda") :: _) =>
        Eval.raise(Error.BadSpecialForm("lambda"))
      case Cons(List(Atom("define"), Atom(varAtom), expr)) =>
        for {
          evalVar <- run(expr)
          env <- ReaderT.ask[IO, EnvCtx]
          updated = env + (varAtom -> evalVar)
          result <- Eval.pure(Atom(varAtom)).local(_ => updated)
        } yield result
      case Cons(Atom(x) :: xs) =>
        for {
          head <- run(Atom(x))
          args   <- xs.traverse(run)
          result <- head match
            case Fun(Function(fn)) => fn(args)
            case Lambda(Function(fn), env) => fn(args).local(_ => env)
            case _ => Eval.raise(Error.NotFunction(head))
        } yield result
      case value =>
        Eval.raise(Error.BadSpecialForm(value.show))

  def getVar(name: String): Eval[Value] =
    ReaderT.ask[IO, EnvCtx].flatMap(env => env.get(name).fold(Eval.raise(Error.UnboundVar(name)))(Eval.pure))
  
  def evalIf(pred: Value, trueExpr: Value, falseExpr: Value): Eval[Value] =
    for {
      predResult <- run(pred)
      result <- predResult match
        case Value.Bool(b) => if (b) run(trueExpr) else run(falseExpr)
        case other => Eval.raise[Value](Error.BadSpecialForm("if"))
    } yield result
  
  def getEven[A](as: List[A]): List[A] =
    as match
      case Nil => Nil
      case a :: tail => a :: getOdd(tail)
  
  def getOdd[A](as: List[A]): List[A] =
    as match
      case Nil => Nil
      case _ :: tail => getEven(tail)
  
  private def ensureAtom(value: Value): Eval[String] =
    value match
      case atom: Value.Atom => Eval.pure(atom.value)
      case other => Eval.raise(Error.TypeMismatch("atom", other))
  
  def evalBody(value: Value): Eval[Value] =
    import Value.*
    value match
      case Cons(List(Cons(List(Atom("define"), Atom(v), defExpr)), rest)) =>
        for {
          evalVal <- run(defExpr)
          env <- ReaderT.ask[IO, EnvCtx]
          result <- run(rest).local(_ => env + (v -> evalVal))
        } yield result
      case Cons(Cons(List(Atom("define"), Atom(v), defExpr)) :: rest) =>
        for {
          evalVal <- run(defExpr)
          env <- ReaderT.ask[IO, EnvCtx]
          result <- evalBody(Cons(rest)).local(_ => env + (v -> evalVal))
        } yield result
      case value =>
        run(value)
  
  def evalLet(pairs: List[Value], expr: Value.Cons): Eval[Value] =
    for {
      env <- ReaderT.ask[IO, EnvCtx]
      atoms <- getEven(pairs).traverse(ensureAtom)
      vals  <- getOdd(pairs).traverse(run)
      updated = atoms.zip(vals).toMap ++ env
      result <- evalBody(expr).local(_ => updated)
    } yield result
  
  def applyLambda(expr: Value, params: List[Value.Atom])(args: List[Value]): Eval[Value] =
    for {
      env <- ReaderT.ask[IO, EnvCtx]
      argEval <- args.traverse(run)
      updated = params.map(_.value).zip(argEval).toMap ++ env
      result <- run(expr).local(_ => updated)
    } yield result

