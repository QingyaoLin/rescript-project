module BigStep: {
  type rec expr = Add(expr, expr) | Mul(expr, expr) | Cst(int)
  let eval: expr => int
}
module SmallStep: {
  type instr = Add | Mul | Cst(int)
  type instrs = list<instr>
  type operand = int
  type stacks = list<operand>
  let eval: (list<instr>, stacks) => operand
}
module Compiler: {
  let compile: BigStep.expr => SmallStep.instrs
}
module ToString: {
  let tostring: BigStep.expr => string
}
module Test: {
  let check_compile: BigStep.expr => unit
  let test: unit => unit
}
