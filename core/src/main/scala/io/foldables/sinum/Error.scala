package io.foldables.sinum

enum Error extends Throwable:
  case UnboundVar(name: String)
  case BadSpecialForm(atom: String)
  case TypeMismatch(expected: String, value: Value)
  case NotFunction(value: Value)

  case ArgumentMismatch(function: String, expected: Int, got: Int)
  case RuntimeError(message: String)

  case ParseError(message: String)
