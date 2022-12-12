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

  let assoc = (variable: string, env: list<(string, int)>) => {
    Belt.Option.getExn(env->Belt.List.getAssoc(variable, (k, item) => k == item))
  }

  let rec eval = (expr, env: env): int => {
    switch expr {
    | Cst(i) => i
    | Add(expr1, expr2) => eval(expr1, env) + eval(expr2, env)
    | Mul(expr1, expr2) => eval(expr1, env) * eval(expr2, env)
    | Var(variable) => assoc(variable, env)
    | Let(variable, expr1, expr2) => eval(expr2, list{(variable, eval(expr1, env)), ...env})
    }
  }

  let rec tostring = (expr): string => {
    switch expr {
    | Cst(i) => j`Ir0.Cst($i)`
    | Add(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        `Ir0.Add(${s1}, ${s2})`
      }

    | Mul(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        `Ir0.Mul(${s1}, ${s2})`
      }

    | Var(variable) => `Ir0.Var("${variable}")`
    | Let(variable, expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        j`Ir0.Let("${variable}", ${s1}, ${s2})`
      }
    }
  }
}

module Ir1 = {
  type env = list<int>

  type rec expr =
    | Add(expr, expr)
    | Mul(expr, expr)
    | Cst(int)
    | Var(int)
    | Let(expr, expr)

  let rec eval = (expr, env) => {
    switch expr {
    | Cst(i) => i
    | Add(expr1, expr2) => eval(expr1, env) + eval(expr2, env)
    | Mul(expr1, expr2) => eval(expr1, env) * eval(expr2, env)
    | Var(n) => List.nth(env, n)
    | Let(expr1, expr2) => eval(expr2, list{eval(expr1, env), ...env})
    }
  }

  let rec tostring = (expr): string => {
    switch expr {
    | Cst(i) => j`Ir1.Cst($i)`
    | Add(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        `Ir1.Add(${s1}, ${s2})`
      }

    | Mul(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        `Ir1.Mul(${s1}, ${s2})`
      }

    | Var(index) => j`Ir1.Var($index)`
    | Let(expr1, expr2) => {
        let s1 = tostring(expr1)
        let s2 = tostring(expr2)
        j`Ir1.Let(${s1}, ${s2})`
      }
    }
  }
}

module ConvertIr0 = {
  type cenv = list<string>

  let index = (s: string, list: list<string>): int => {
    let count = ref(0)

    for i in 0 to Belt.List.length(list) - 1 {
      if s != Belt.Option.getExn(list->Belt.List.get(i)) {
        count := i
      }
    }

    count.contents
  }

  let rec convert = (expr: Ir0.expr, env: cenv): Ir1.expr => {
    switch expr {
    | Cst(i) => Ir1.Cst(i)
    | Add(expr1, expr2) => Ir1.Add(convert(expr1, env), convert(expr2, env))
    | Mul(expr1, expr2) => Ir1.Mul(convert(expr1, env), convert(expr2, env))
    | Var(variable) => Ir1.Var(index(variable, env))

    | Let(variable, expr1, expr2) =>
      Ir1.Let(convert(expr1, env), convert(expr2, list{variable, ...env}))
    }
  }
}

// Ir1 => Instr0
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

  let addressing = (index, stack: stack) => {
    Belt.Option.getExn(stack->Belt.List.get(index))
  }

  let pop = (stack: stack) => {
    Belt.List.tailExn(stack)
  }

  let rec eval = (instrs: instrs, stack: stack): int => {
    switch (instrs, stack) {
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})
    | (list{Add, ...rest}, list{operand1, operand2, ...stack}) =>
      eval(rest, list{operand1 + operand2, ...stack})
    | (list{Mul, ...rest}, list{operand1, operand2, ...stack}) =>
      eval(rest, list{operand1 * operand2, ...stack})
    | (list{Var(n), ...rest}, _) => eval(rest, list{addressing(n, stack), ...stack})
    | (list{Pop, ...rest}, _) => eval(rest, pop(stack))
    | (list{Swap, ...rest}, list{operand1, operand2, ...stack}) =>
      eval(rest, list{operand2, operand1, ...stack})
    | (list{}, list{result, ..._}) => result
    | _ => assert false
    }
  }

  // let rec tostring = instrs => {

  // }
}

// Let("x", Cst(1), Add(Var("x"),Var("x")))
// =>
// [Cst(1);Var(0);Var(1);Swap;Pop]

module Compiler = {
  // Ir1 => Intrs1
  let rec compileIr1 = (expr: Ir1.expr): Instr1.instrs => {
    switch expr {
    | Cst(i) => list{Cst(i)}
    | Add(expr1, expr2) => {
        let target1 = compileIr1(expr1)
        let target2 = compileIr1(expr2)
        Belt.List.concatMany([target1, target2, list{Add}])
      }

    | Mul(expr1, expr2) => {
        let target1 = compileIr1(expr1)
        let target2 = compileIr1(expr2)
        Belt.List.concatMany([target1, target2, list{Mul}])
      }

    | Var(n) => list{Var(n)}
    | Let(expr1, expr2) => {
        let target1 = compileIr1(expr1)
        let target2 = compileIr1(expr2)
        Belt.List.concatMany([target1, target2, list{Swap, Pop}])
      }
    }
  }
}

module Test = {
  let check_convert = (expr: Ir0.expr) => {
    let ir0_result = Ir0.eval(expr, list{})

    let ir1_expr = ConvertIr0.convert(expr, list{})

    let ir1_result = Ir1.eval(ir1_expr, list{})

    assert (ir0_result == ir1_result)
  }

  let check_compile = (expr: Ir0.expr) => {
    let ir1_expr = ConvertIr0.convert(expr, list{})
    let ir1_result = Ir1.eval(ir1_expr, list{})

    let instrs0 = Compiler.compileIr1(ir1_expr)
    let instr_result = Instr1.eval(instrs0, list{})

    assert (ir1_result == instr_result)
  }

  let test_convert = () => {
    let expr_array = [
      Ir0.Cst(10),
      Add(Cst(10), Let("x", Cst(20), Add(Var("x"), Cst(50)))),
      Mul(Cst(10), Let("x", Cst(20), Add(Var("x"), Cst(50)))),
      Let("x", Let("x", Cst(1), Add(Var("x"), Var("x"))), Var("x")),
      Let("x", Cst(10), Let("x", Add(Var("x"), Var("x")), Var("x"))),
      Let("x", Cst(1), Add(Var("x"), Var("x"))),
    ]

    Belt.Array.forEachWithIndex(expr_array, (index, expr) => {
      check_convert(expr)
      Js.log(j`Test $index: ` ++ "Ir0 convert to Ir1 is passed!")
    })

    Js.log("")
  }

  let test_instr1 = () => {
    let instrs = list{Instr1.Cst(10), Var(0), Add, Var(0), Swap, Pop}
    let result = Instr1.eval(instrs, list{})

    assert (result == 20)

    Js.log("Test test_instr0 passed!\n")
  }

  let test_compile = () => {
    let expr_array = [
      Ir0.Cst(10),
      Add(Cst(10), Let("x", Cst(20), Add(Var("x"), Cst(50)))),
      Mul(Cst(10), Let("x", Cst(20), Add(Var("x"), Cst(50)))),
      Let("x", Let("x", Cst(1), Add(Var("x"), Var("x"))), Var("x")),
      Let("x", Cst(10), Let("x", Add(Var("x"), Var("x")), Var("x"))),
      Let("x", Cst(1), Add(Var("x"), Var("x"))),
    ]

    Belt.Array.forEachWithIndex(expr_array, (index, expr) => {
      check_compile(expr)
      Js.log(j`Test $index: ` ++ "Ir1 compile to Instr1 is passed!")
    })

    Js.log("")
  }
}

Test.test_convert()
Test.test_instr1()
Test.test_compile()
