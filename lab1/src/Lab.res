// HomeWork1:
// 1. Ir0 => Ir1
//    - eval      √
//    - tostring  √
// 2. Ir1 => Instrs1
//    - eval      √
//    - tostring  √

// HomeWork2:
// 1. Ir0     => Instrs0
//    - eval      √
//    - tostring  √
// 2. Instrs0 => Instrs1
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

// 带有环境的指令集
// semantics:
//    (Var(x);c, s, e)   -->  (c, e[x]::s, e)
//  (Let(x);c, v::s, e)  --> (c, s, (x, v)::e)
module Instrs0 = {
  type instr =
    | Add
    | Mul
    | Cst(int)
    | Var(string) // 视为临时变量
    | Let(string) // 视为局部变量
    | Pop
    | Swap

  type instrs = list<instr>
  type stack = list<int>
  type senv = list<(string, int)>

  let pop = (stack: stack) => {
    Belt.List.tailExn(stack)
  }

  let eval = instrs => {
    let rec interpret = (instrs: instrs, stack: stack, senv: senv): int => {
      switch (instrs, stack) {
      | (list{Cst(i), ...rest}, _) => interpret(rest, list{i, ...stack}, senv)
      | (list{Add, ...rest}, list{operand1, operand2, ...stack}) =>
        interpret(rest, list{operand1 + operand2, ...stack}, senv)
      | (list{Mul, ...rest}, list{operand1, operand2, ...stack}) =>
        interpret(rest, list{operand1 * operand2, ...stack}, senv)
      | (list{Let(local), ...rest}, list{value, ..._}) =>
        interpret(rest, stack, list{(local, value), ...senv})
      | (list{Var(tmp), ...rest}, _) => interpret(rest, list{List.assoc(tmp, senv), ...stack}, senv)
      | (list{Pop, ...rest}, _) => interpret(rest, pop(stack), senv)
      | (list{Swap, ...rest}, list{operand1, operand2, ...stack}) =>
        interpret(rest, list{operand2, operand1, ...stack}, senv)
      | (list{}, list{result}) => result
      | _ => assert false
      }
    }

    interpret(instrs, list{}, list{})
  }

  let rec tostring = instrs => {
    switch instrs {
    | list{Cst(i), ...rest} => j`Cst($i);` ++ tostring(rest)
    | list{Add, ...rest} => "Add;" ++ tostring(rest)
    | list{Mul, ...rest} => "Mul;" ++ tostring(rest)
    | list{Var(tmp), ...rest} => `Var(${tmp});` ++ tostring(rest)
    | list{Let(local), ...rest} => `Let(${local});` ++ tostring(rest)
    | list{Pop, ...rest} => "Pop;" ++ tostring(rest)
    | list{Swap, ...rest} => "Swap;" ++ tostring(rest)
    | list{} => ""
    }
  }
}

// 相关semantic:
//          (Var(n);c, s) --> (c, s[n]::s)
//          (Pop;c, v::s) --> (c, s)
//    (Swap;c, v1::v2::s) --> (c, V2::v1::s)
module Instrs1 = {
  type instr =
    | Add
    | Mul
    | Cst(int)
    | Var(int)
    | Pop
    | Swap

  type instrs = list<instr>
  type stack = list<int>

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
      | (list{}, list{result}) => result
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

module Interpreter = {
  type cenv = list<string>

  // Ir0 => Ir1
  let convertIr0 = expr => {
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

  // Ir0 => Instr0
  let convertInstrs0 = expr => {
    let rec compile = (expr: Ir0.expr): Instrs0.instrs => {
      switch expr {
      | Cst(i) => list{Instrs0.Cst(i)}
      | Var(variable) => list{Var(variable)}
      | Let(variable, expr1, expr2) =>
        Belt.List.concatMany([compile(expr1), list{Let(variable)}, compile(expr2), list{Swap, Pop}])
      | Add(expr1, expr2) => Belt.List.concatMany([compile(expr1), compile(expr2), list{Add}])
      | Mul(expr1, expr2) => Belt.List.concatMany([compile(expr1), compile(expr2), list{Mul}])
      }
    }
    compile(expr)
  }
}

// Ir0 => Instr0 => Instr1
module Compile0 = {
  // 是临时，还是本地变量,且变量名为
  type varType = Temp | Local(string)

  let compile = (expr: Ir0.expr) => {
    let toInstrs0 = Interpreter.convertInstrs0(expr)

    let rec toInstrs1 = (instrs, cenv: list<varType>): Instrs1.instrs => {
      switch instrs {
      | list{Instrs0.Cst(i), ...rest} => list{Instrs1.Cst(i), ...toInstrs1(rest, cenv)}
      | list{Add, ...rest} => list{Instrs1.Add, ...toInstrs1(rest, cenv)}
      | list{Mul, ...rest} => list{Instrs1.Mul, ...toInstrs1(rest, cenv)}
      | list{Var(variable), ...rest} =>
        list{Var(index(cenv, Local(variable), 0)), ...toInstrs1(rest, list{Temp, ...cenv})}
      | list{Let(variable), ...rest} => toInstrs1(rest, list{Local(variable), ...cenv})
      | list{Swap, ...rest} => list{Instrs1.Swap, ...toInstrs1(rest, cenv)}
      | list{Pop, ...rest} => list{Instrs1.Pop, ...toInstrs1(rest, cenv)}
      | _ => list{}
      }
    }

    toInstrs1(toInstrs0, list{})
  }
}

// Ir0 => Ir1 => Instr1
// Let("x", Cst(1), Add(Var("x"),Var("x")))
// =>
// Let(Cst(1), Add(Var(0),Var(0)))
// =>
// [Cst(1);Var(0);Var(1);Add;Swap;Pop]
module Compile1 = {
  // 是临时，还是本地变量,且变量名为
  type varType = Temp | Local

  let compile = (expr: Ir0.expr) => {
    let toIr1 = Interpreter.convertIr0(expr)

    let rec toInstrs1 = (expr, cenv: list<varType>): Instrs1.instrs => {
      switch expr {
      | Ir1.Cst(i) => list{Instrs1.Cst(i)}
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

    let instrs0 = list{Instrs0.Cst(1), Let("x"), Var("x"), Var("x"), Add, Swap, Pop}
    let instrs1 = list{Instrs1.Cst(1), Var(0), Var(1), Add, Swap, Pop}

    let ir1_str = Ir1.tostring(ir1)
    let instrs0_str = Instrs0.tostring(instrs0)
    let instrs1_str = Instrs1.tostring(instrs1)

    let ir0_result = Ir0.eval(ir0)
    let ir1_result = Ir1.eval(ir1)
    let instrs0_result = Instrs0.eval(instrs0)
  }

  module Exple2 = {
    let ir0 = Ir0.Add(Cst(1), Let("x", Cst(2), Add(Var("x"), Cst(7))))
    let ir1 = Ir1.Add(Cst(1), Let(Cst(2), Add(Var(0), Cst(7))))
    let instrs1 = list{Instrs1.Cst(1), Cst(2), Var(0), Cst(7), Add, Swap, Pop, Add}

    let ir1_str = Ir1.tostring(ir1)
    let instrs1_str = Instrs1.tostring(instrs1)

    let ir0_result = Ir0.eval(ir0)
    let ir1_result = Ir1.eval(ir1)
  }

  // 检查 解释器实现 是否正确
  let check_interpreter = () => {
    // test Ir0 => Ir1
    let test_convertIr0 = () => {
      // test Exple1
      let ir0_to_ir1 = Interpreter.convertIr0(Exple1.ir0)

      assert (Exple1.ir1_str == Ir1.tostring(ir0_to_ir1))
      assert (Exple1.ir1_result == Ir1.eval(ir0_to_ir1))

      // test Exple2
      let ir0_to_ir1 = Interpreter.convertIr0(Exple2.ir0)

      assert (Exple2.ir1_str == Ir1.tostring(ir0_to_ir1))
      assert (Exple2.ir1_result == Ir1.eval(ir0_to_ir1))
      Js.log("test_convertIr0() is successful!")
    }

    // test Ir0 => Instrs0
    let test_convertInstrs0 = () => {
      // test Exple1
      let ir0_to_instrs0 = Interpreter.convertInstrs0(Exple1.ir0)

      assert (Exple1.instrs0_str == Instrs0.tostring(ir0_to_instrs0))
      assert (Exple1.instrs0_result == Instrs0.eval(ir0_to_instrs0))

      Js.log("test_convertInstrs0() is successful!")
    }

    test_convertIr0()
    test_convertInstrs0()
  }

  let check_compiler = () => {
    // Ir0 => Instrs0 => Instrs1
    let test_compile0 = () => {
      // test Exple1
      let instrs1 = Compile0.compile(Exple1.ir0)

      assert (Instrs1.eval(instrs1) == Exple1.instrs0_result)
      assert (Instrs1.tostring(instrs1) == Exple1.instrs1_str)

      // test Exple2
      let instrs1 = Compile0.compile(Exple2.ir0)

      assert (Instrs1.eval(instrs1) == Exple2.ir0_result)
      assert (Instrs1.tostring(instrs1) == Exple2.instrs1_str)

      Js.log("test_compile0() is successful!")
    }

    // Ir0 => Ir1 => Instrs1
    let test_compile1 = () => {
      // test Exple1
      let instrs1 = Compile1.compile(Exple1.ir0)

      assert (Instrs1.eval(instrs1) == Exple1.ir0_result)
      assert (Instrs1.tostring(instrs1) == Exple1.instrs1_str)

      // test Exple2
      let instrs1 = Compile1.compile(Exple2.ir0)

      assert (Instrs1.eval(instrs1) == Exple2.ir0_result)
      assert (Instrs1.tostring(instrs1) == Exple2.instrs1_str)

      Js.log("test_compile1() is successful!")
    }

    test_compile0()
    test_compile1()
  }

  let test = () => {
    check_interpreter()
    Js.log("")
    check_compiler()
  }
}

Test.test()
