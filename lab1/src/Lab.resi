module Ir0: {
  type env = list<(string, int)>
  type rec expr = Add(expr, expr) | Mul(expr, expr) | Cst(int) | Var(string) | Let(string, expr, expr)
  let assoc: (string, list<(string, int)>) => int
  let eval: (expr, env) => int
  let tostring: expr => string
}
module Ir1: {
  type env = list<int>
  type rec expr = Add(expr, expr) | Mul(expr, expr) | Cst(int) | Var(int) | Let(expr, expr)
  let eval: (expr, list<int>) => int
  let tostring: expr => string
}
module ConvertIr0: {
  type cenv = list<string>
  let index: (string, list<string>) => int
  let convert: (Ir0.expr, cenv) => Ir1.expr
}
module Test: {
  let check_convert: Ir0.expr => unit
  let test1: unit => unit
}
