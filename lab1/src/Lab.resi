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
module Interpreter0: {
  type cenv = list<string>
  let convert: Ir0.expr => Ir1.expr
}
module Instr1: {
  type instr = Add | Mul | Cst(int) | Var(int) | Pop | Swap
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>
  let get: (int, stack) => operand
  let pop: stack => Belt.List.t<operand>
  let eval: instrs => int
  let tostring: list<instr> => string
}
module Compiler: {
  type varType = Temp | Local
  let compileIr0: Ir0.expr => Instr1.instrs
}
module Test: {
  module Exple1: {
    let ir0: Ir0.expr
    let ir1: Ir1.expr
    let instrs1: list<Instr1.instr>
    let ir1_str: string
    let instrs1_str: string
    let ir0_result: int
    let ir1_result: int
  }
  module Exple2: {
    let ir0: Ir0.expr
    let ir1: Ir1.expr
    let instrs1: list<Instr1.instr>
    let ir1_str: string
    let instrs1_str: string
    let ir0_result: int
    let ir1_result: int
  }
  let check_interpreter: unit => unit
  let check_compiler: unit => unit
  let test: unit => unit
}
