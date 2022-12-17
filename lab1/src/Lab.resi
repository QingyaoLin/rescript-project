module Ir0: {
  type env = list<(string, int)>
  type rec expr = Add(expr, expr) | Mul(expr, expr) | Cst(int) | Var(string) | Let(string, expr, expr)
  let eval: expr => int
  let tostring: expr => string
}
module Ir1: {
  type env = list<int>
  type rec expr = Add(expr, expr) | Mul(expr, expr) | Cst(int) | Var(int) | Let(expr, expr)
  let eval: expr => int
  let tostring: expr => string
}
let index: (list<'a>, 'a, int) => int
module Instrs0: {
  type instr = Add | Mul | Cst(int) | Var(string) | Let(string) | Pop | Swap
  type instrs = list<instr>
  type stack = list<int>
  type senv = list<(string, int)>
  let pop: stack => Belt.List.t<int>
  let eval: instrs => int
  let tostring: list<instr> => string
}
module Instrs1: {
  type instr = Add | Mul | Cst(int) | Var(int) | Pop | Swap
  type instrs = list<instr>
  type stack = list<int>
  let get: (int, stack) => int
  let pop: stack => Belt.List.t<int>
  let eval: instrs => int
  let tostring: list<instr> => string
}
module Interpreter: {
  type cenv = list<string>
  let convertIr0: Ir0.expr => Ir1.expr
  let convertInstrs0: Ir0.expr => Instrs0.instrs
}
module Compile0: {
  type varType = Temp | Local(string)
  let compile: Ir0.expr => Instrs1.instrs
}
module Compile1: {
  type varType = Temp | Local
  let compile: Ir0.expr => Instrs1.instrs
}
module Test: {
  module Exple1: {
    let ir0: Ir0.expr
    let ir1: Ir1.expr
    let instrs0: list<Instrs0.instr>
    let instrs1: list<Instrs1.instr>
    let ir1_str: string
    let instrs0_str: string
    let instrs1_str: string
    let ir0_result: int
    let ir1_result: int
    let instrs0_result: int
  }
  module Exple2: {
    let ir0: Ir0.expr
    let ir1: Ir1.expr
    let instrs1: list<Instrs1.instr>
    let ir1_str: string
    let instrs1_str: string
    let ir0_result: int
    let ir1_result: int
  }
  let check_interpreter: unit => unit
  let check_compiler: unit => unit
  let test: unit => unit
}
