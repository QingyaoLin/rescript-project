// HomeWork:
// 1. Ir0 => Ir1
//    - eval      √
//    - tostring  √
// 2. Ir1 => Instr0
//    - eval      √
//    - tostring  √

// Var(string) 表示使用这个变量，即 GET
// Let(string, expr1, expr2) 表示一个变量绑定表达式:
// example: let x = 2 in x + 7 end
// return:  => 9
module Ir0 = {
  type env = list<(string, int)>

  type rec expr =
    | Add(expr, expr)
    | Mul(expr, expr)
    | Cst(int)
    | Var(string)
    | Let(string, expr, expr)

  let eval = (expr): int => {
    let rec compile = (expr, env) => {
      switch expr {
      | Cst(i) => i
      | Add(expr1, expr2) => compile(expr1, env) + compile(expr2, env)
      | Mul(expr1, expr2) => compile(expr1, env) * compile(expr2, env)
      | Var(variable) => List.assoc(variable, env)
      | Let(variable, expr1, expr2) => compile(expr2, list{(variable, compile(expr1, env)), ...env})
      }
    }
    compile(expr, list{})
  }

  let rec tostring = (expr): string => {
    switch expr {
    | Cst(i) => j`Cst($i)`
    | Add(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        `Add(${s1}, ${s2})`
      }

    | Mul(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        `Mul(${s1}, ${s2})`
      }

    | Var(variable) => `Var("${variable}")`
    | Let(variable, expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        j`Let("${variable}", ${s1}, ${s2})`
      }
    }
  }
}

// 改为通过索引查找变量的值
module Ir1 = {
  type env = list<int>

  type rec expr =
    | Add(expr, expr)
    | Mul(expr, expr)
    | Cst(int)
    | Var(int)
    | Let(expr, expr)

  let eval = expr => {
    let rec compile = (expr, env) => {
      switch expr {
      | Cst(i) => i
      | Add(expr1, expr2) => compile(expr1, env) + compile(expr2, env)
      | Mul(expr1, expr2) => compile(expr1, env) * compile(expr2, env)
      | Var(n) => List.nth(env, n)
      | Let(expr1, expr2) => compile(expr2, list{compile(expr1, env), ...env})
      }
    }
    compile(expr, list{})
  }

  let rec tostring = (expr): string => {
    switch expr {
    | Cst(i) => j`Cst($i)`
    | Add(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        `Add(${s1}, ${s2})`
      }

    | Mul(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        `Mul(${s1}, ${s2})`
      }

    | Var(index) => j`Var($index)`
    | Let(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        j`Let(${s1}, ${s2})`
      }
    }
  }
}

let rec index = (l: list<'a>, x: 'a, n: int) => {
  switch (l, n) {
  | (list{hd, ...rest}, 0) =>
    if hd == x {
      0
    } else {
      1 + index(rest, x, 0)
    }

  | (list{hd, ...rest}, n) =>
    if hd == x {
      1 + index(rest, x, n - 1)
    } else {
      1 + index(rest, x, n)
    }

  | _ => assert false
  }
}

// 将 Ir0 => Ir1
module Interpreter0 = {
  type cenv = list<string>

  let convert = expr => {
    let rec compile = (expr: Ir0.expr, env: cenv): Ir1.expr => {
      switch expr {
      | Cst(i) => Ir1.Cst(i)
      | Add(expr1, expr2) => Ir1.Add(compile(expr1, env), compile(expr2, env))
      | Mul(expr1, expr2) => Ir1.Mul(compile(expr1, env), compile(expr2, env))
      | Var(variable) => Ir1.Var(index(env, variable, 0))

      | Let(variable, expr1, expr2) =>
        Ir1.Let(compile(expr1, env), compile(expr2, list{variable, ...env}))
      }
    }
    compile(expr, list{})
  }
}

// Ir1 => Instr1
// 相关semantic:
//          (Var(n);c, s) --> (c, s[n]::s)
//          (Pop;c, v::s) --> (c, s)
//    (Swap;c, v1::v2::s) --> (c, V2::v1::s)
module Instr1 = {
  type instr =
    | Add
    | Mul
    | Cst(int)
    | Var(int)
    | Pop
    | Swap

  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  let get = (index, stack: stack) => {
    List.nth(stack, index)
  }

  let pop = (stack: stack) => {
    Belt.List.tailExn(stack)
  }

  let eval = instrs => {
    let rec interpret = (instrs: instrs, stack: stack): int => {
      switch (instrs, stack) {
      | (list{Cst(i), ...rest}, _) => interpret(rest, list{i, ...stack})
      | (list{Add, ...rest}, list{operand1, operand2, ...stack}) =>
        interpret(rest, list{operand1 + operand2, ...stack})
      | (list{Mul, ...rest}, list{operand1, operand2, ...stack}) =>
        interpret(rest, list{operand1 * operand2, ...stack})
      | (list{Var(n), ...rest}, _) => interpret(rest, list{get(n, stack), ...stack})
      | (list{Pop, ...rest}, _) => interpret(rest, pop(stack))
      | (list{Swap, ...rest}, list{operand1, operand2, ...stack}) =>
        interpret(rest, list{operand2, operand1, ...stack})
      | (list{}, list{result, ..._}) => result
      | _ => assert false
      }
    }

    interpret(instrs, list{})
  }

  let rec tostring = instrs => {
    switch instrs {
    | list{Cst(i), ...rest} => j`Cst($i);` ++ tostring(rest)
    | list{Add, ...rest} => "Add;" ++ tostring(rest)
    | list{Mul, ...rest} => "Mul;" ++ tostring(rest)
    | list{Var(n), ...rest} => j`Var($n);` ++ tostring(rest)
    | list{Pop, ...rest} => "Pop;" ++ tostring(rest)
    | list{Swap, ...rest} => "Swap;" ++ tostring(rest)
    | list{} => ""
    }
  }
}

module Compiler = {
  // 是临时，还是本地变量
  type varType = Temp | Local

  // Ir0 => Ir1 => Instr1
  // Let("x", Cst(1), Add(Var("x"),Var("x")))
  // =>
  // Let(Cst(1), Add(Var(0),Var(0)))
  // =>
  // [Cst(1);Var(0);Var(1);Add;Swap;Pop]
  let compileIr0 = (expr: Ir0.expr) => {
    let toIr1 = Interpreter0.convert(expr)

    let rec toInstrs1 = (expr, cenv: list<varType>): Instr1.instrs => {
      switch expr {
      | Ir1.Cst(i) => list{Instr1.Cst(i)}
      | Add(expr1, expr2) => {
          let target1 = toInstrs1(expr1, cenv)
          let target2 = toInstrs1(expr2, list{Temp, ...cenv})
          Belt.List.concatMany([target1, target2, list{Add}])
        }

      | Mul(expr1, expr2) => {
          let target1 = toInstrs1(expr1, cenv)
          let target2 = toInstrs1(expr2, list{Temp, ...cenv})
          Belt.List.concatMany([target1, target2, list{Mul}])
        }

      | Var(n) => list{Var(index(cenv, Local, n))}
      | Let(expr1, expr2) => {
          let target1 = toInstrs1(expr1, cenv)
          let target2 = toInstrs1(expr2, list{Local, ...cenv})
          Belt.List.concatMany([target1, target2, list{Swap, Pop}])
        }
      }
    }

    toInstrs1(toIr1, list{})
  }
}

module Test = {
  module Exple1 = {
    let ir0 = Ir0.Let("x", Cst(1), Add(Var("x"), Var("x")))
    let ir1 = Ir1.Let(Cst(1), Add(Var(0), Var(0)))
    let instrs1 = list{Instr1.Cst(1), Var(0), Var(1), Add, Swap, Pop}

    let ir1_str = Ir1.tostring(ir1)
    let instrs1_str = Instr1.tostring(instrs1)

    let ir0_result = Ir0.eval(ir0)
    let ir1_result = Ir1.eval(ir1)
  }

  module Exple2 = {
    let ir0 = Ir0.Add(Cst(1), Let("x", Cst(2), Add(Var("x"), Cst(7))))
    let ir1 = Ir1.Add(Cst(1), Let(Cst(2), Add(Var(0), Cst(7))))
    let instrs1 = list{Instr1.Cst(1), Cst(2), Var(0), Cst(7), Add, Swap, Pop, Add}

    let ir1_str = Ir1.tostring(ir1)
    let instrs1_str = Instr1.tostring(instrs1)

    let ir0_result = Ir0.eval(ir0)
    let ir1_result = Ir1.eval(ir1)
  }

  // 检查 解释器实现 是否正确
  let check_interpreter = () => {
    let test_interpreter0 = () => {
      // test Exple1
      let ir0_to_ir1 = Interpreter0.convert(Exple1.ir0)

      assert (Exple1.ir1_str == Ir1.tostring(ir0_to_ir1))
      assert (Exple1.ir1_result == Ir1.eval(ir0_to_ir1))

      // test Exple2
      let ir0_to_ir1 = Interpreter0.convert(Exple2.ir0)

      assert (Exple2.ir1_str == Ir1.tostring(ir0_to_ir1))
      assert (Exple2.ir1_result == Ir1.eval(ir0_to_ir1))
      Js.log("test_interpreter0() is successful!")
    }

    test_interpreter0()
  }

  let check_compiler = () => {
    // Ir0 => Ir1 => Instr1
    let test_compileIr0 = () => {
      // test Exple1
      let instrs1 = Compiler.compileIr0(Exple1.ir0)

      assert (Instr1.eval(instrs1) == Exple1.ir0_result)
      assert (Instr1.tostring(instrs1) == Exple1.instrs1_str)

      // test Exple2
      let instrs1 = Compiler.compileIr0(Exple2.ir0)

      assert (Instr1.eval(instrs1) == Exple2.ir0_result)
      assert (Instr1.tostring(instrs1) == Exple2.instrs1_str)

      Js.log("test_compileIr0() is successful!")
    }

    test_compileIr0()
  }

  let test = () => {
    check_interpreter()
    check_compiler()
  }
}

Test.test()
