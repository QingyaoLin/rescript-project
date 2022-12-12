// Semantics:
// - terms:   e ::= Cst(i) | Add(e1,e2) | Mul(e1,e2)
// - values:  v ::= i ∈ Int
//
// BNF 范式（巴科斯范式）表示语法:
//
// expr := INT
//      | expr "+" expr
//      | expr "*" expr
//      | "(" expr ")"

module BigStep = {
  type rec expr = Add(expr, expr) | Mul(expr, expr) | Cst(int)

  let rec eval = expr => {
    switch expr {
    | Cst(i) => i
    | Add(expr1, expr2) => eval(expr1) + eval(expr2)
    | Mul(expr1, expr2) => eval(expr1) * eval(expr2)
    }
  }
}

// 模拟栈虚拟机的执行语义：
// - code:  c ::= None | i;c
// - stack: s ::= None | v::s
//
// Transition of the machine:
//         (Cst(i);c,s) -> (c,i::s)
//    (Add;c,n2::n1::s) -> (c,(n2 + n1)::s)
//    (Mul;c,n2::n1::s) -> (c,(n2 * n1)::s)
module SmallStep = {
  type instr = Add | Mul | Cst(int)
  type instrs = list<instr>
  type operand = int
  type stacks = list<operand>

  let rec eval = (instrs, stack: stacks) => {
    switch (instrs, stack) {
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})
    | (list{Add, ...rest}, list{operand1, operand2, ...stack}) =>
      eval(rest, list{operand1 + operand2, ...stack})
    | (list{Mul, ...rest}, list{operand1, operand2, ...stack}) =>
      eval(rest, list{operand1 * operand2, ...stack})
    | (list{}, list{result, ..._}) => result
    | _ => assert false
    }
  }
}

// 从形式化的高级语义编译为低级的指令语义:
//    Bigstep ==> Smallstep
//
//        [Cst(i)] = Cst(i)
//    [Add(e1,e2)] = [e1] ; [e2] ; Add
//    [Mul(e1,e2)] = [e1] ; [e2] ; Mul
//
// Notes: `[...]` 表示为编译过程
module Compiler = {
  let rec compile = (src: BigStep.expr): SmallStep.instrs => {
    switch src {
    | Cst(i) => list{Cst(i)}
    | Add(expr1, expr2) => {
        let target1 = compile(expr1)
        let target2 = compile(expr2)
        Belt.List.concatMany([target1, target2, list{Add}])
      }

    | Mul(expr1, expr2) => {
        let target1 = compile(expr1)
        let target2 = compile(expr2)
        Belt.List.concatMany([target1, target2, list{Mul}])
      }
    }
  }
}

// helper function
module ToString = {
  let rec tostring = (expr): string => {
    switch expr {
    | BigStep.Cst(i) => j`Cst($i)`
    | Add(expr1, expr2) => {
        let expr1_str = tostring(expr1)
        let expr2_str = tostring(expr2)
        `Add(${expr1_str}, ${expr2_str})`
      }

    | Mul(expr1, expr2) => {
        let expr1_str = tostring(expr1)
        let expr2_str = tostring(expr2)
        `Mul(${expr1_str}, ${expr2_str})`
      }
    }
  }
}

// 1.2章节 验证: 在高级层面上的结果等价于栈虚拟机指令上的结果
//        e ↓ v <==> [e] ↓ v
module Test = {
  let check_compile = (src: BigStep.expr) => {
    let instrs = Compiler.compile(src)
    let result = SmallStep.eval(instrs, list{})
    assert (result == BigStep.eval(src))
  }

  let test = () => {
    let tests = [
      BigStep.Cst(42),
      Add(Cst(1), Cst(2)),
      Mul(Cst(1), Cst(2)),
      Add(Mul(Cst(1), Cst(2)), Cst(3)),
      Mul(Mul(Cst(1), Cst(2)), Cst(3)),
    ]

    Belt.Array.forEachWithIndex(tests, (i, t) => {
      check_compile(t)
      let expr_str = ToString.tostring(tests[i])

      Js.log(`Test ${expr_str} pass...`)
    })
  }
}

Test.test()
