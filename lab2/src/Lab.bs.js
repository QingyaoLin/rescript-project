// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";

function vadd(v1, v2) {
  if (v1.TAG === /* Vint */0) {
    if (v2.TAG === /* Vint */0) {
      return {
              TAG: /* Vint */0,
              _0: v1._0 + v2._0 | 0
            };
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "Lab.res",
            34,
            11
          ],
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Lab.res",
          34,
          11
        ],
        Error: new Error()
      };
}

function vmul(v1, v2) {
  if (v1.TAG === /* Vint */0) {
    if (v2.TAG === /* Vint */0) {
      return {
              TAG: /* Vint */0,
              _0: Math.imul(v1._0, v2._0)
            };
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "Lab.res",
            41,
            11
          ],
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Lab.res",
          41,
          11
        ],
        Error: new Error()
      };
}

function capture(_closure_env, parent_env, _body) {
  while(true) {
    var body = _body;
    var closure_env = _closure_env;
    switch (body.TAG | 0) {
      case /* Var */1 :
          var variable = body._0;
          if (Belt_List.has(closure_env, variable, (function (k, item) {
                    return k === item;
                  }))) {
            return /* [] */0;
          }
          if (Belt_List.hasAssoc(parent_env, variable, (function (k, item) {
                    return k === item;
                  }))) {
            return {
                    hd: [
                      variable,
                      List.assoc(variable, parent_env)
                    ],
                    tl: /* [] */0
                  };
          }
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "Lab.res",
                  54,
                  8
                ],
                Error: new Error()
              };
      case /* Add */2 :
      case /* Mul */3 :
          break;
      case /* Fn */4 :
          var closure_env$1 = Belt_List.concat(body._0, closure_env);
          _body = body._1;
          _closure_env = closure_env$1;
          continue ;
      case /* Cst */0 :
      case /* Call */5 :
          return /* [] */0;
      case /* Let */6 :
          return Belt_List.concat(capture({
                          hd: body._0,
                          tl: closure_env
                        }, parent_env, body._2), capture(closure_env, parent_env, body._1));
      
    }
    return Belt_List.concat(capture(closure_env, parent_env, body._1), capture(closure_env, parent_env, body._0));
  };
}

function interpret(expr) {
  var $$eval = function (_expr, _env) {
    while(true) {
      var env = _env;
      var expr = _expr;
      switch (expr.TAG | 0) {
        case /* Cst */0 :
            return {
                    TAG: /* Vint */0,
                    _0: expr._0
                  };
        case /* Var */1 :
            return List.assoc(expr._0, env);
        case /* Add */2 :
            return vadd($$eval(expr._0, env), $$eval(expr._1, env));
        case /* Mul */3 :
            return vmul($$eval(expr._0, env), $$eval(expr._1, env));
        case /* Fn */4 :
            var body = expr._1;
            var pars = expr._0;
            var capture_env = capture(pars, env, body);
            return {
                    TAG: /* Vclosure */1,
                    _0: capture_env,
                    _1: pars,
                    _2: body
                  };
        case /* Call */5 :
            var match = $$eval(expr._0, env);
            if (match.TAG === /* Vint */0) {
              throw {
                    RE_EXN_ID: "Match_failure",
                    _1: [
                      "Lab.res",
                      89,
                      14
                    ],
                    Error: new Error()
                  };
            }
            var args_value = Belt_List.map(expr._1, (function(env){
                return function (arg) {
                  return $$eval(arg, env);
                }
                }(env)));
            var fun_env = Belt_List.concat(Belt_List.zip(match._1, args_value), match._0);
            _env = fun_env;
            _expr = match._2;
            continue ;
        case /* Let */6 :
            _env = {
              hd: [
                expr._0,
                $$eval(expr._1, env)
              ],
              tl: env
            };
            _expr = expr._2;
            continue ;
        
      }
    };
  };
  var vint = $$eval(expr, /* [] */0);
  if (vint.TAG === /* Vint */0) {
    return vint._0;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Lab.res",
          103,
          13
        ],
        Error: new Error()
      };
}

var Ir0 = {
  vadd: vadd,
  vmul: vmul,
  capture: capture,
  interpret: interpret
};

function vadd$1(v1, v2) {
  if (v1.TAG === /* Vint */0) {
    if (v2.TAG === /* Vint */0) {
      return {
              TAG: /* Vint */0,
              _0: v1._0 + v2._0 | 0
            };
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "Lab.res",
            134,
            11
          ],
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Lab.res",
          134,
          11
        ],
        Error: new Error()
      };
}

function vmul$1(v1, v2) {
  if (v1.TAG === /* Vint */0) {
    if (v2.TAG === /* Vint */0) {
      return {
              TAG: /* Vint */0,
              _0: Math.imul(v1._0, v2._0)
            };
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "Lab.res",
            141,
            11
          ],
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Lab.res",
          141,
          11
        ],
        Error: new Error()
      };
}

function interpret$1(expr) {
  var $$eval = function (_expr, _env) {
    while(true) {
      var env = _env;
      var expr = _expr;
      switch (expr.TAG | 0) {
        case /* Cst */0 :
            return {
                    TAG: /* Vint */0,
                    _0: expr._0
                  };
        case /* Var */1 :
            return List.nth(env, expr._0);
        case /* Add */2 :
            return vadd$1($$eval(expr._0, env), $$eval(expr._1, env));
        case /* Mul */3 :
            return vmul$1($$eval(expr._0, env), $$eval(expr._1, env));
        case /* Fn */4 :
            return {
                    TAG: /* Vclosure */1,
                    _0: env,
                    _1: expr._0
                  };
        case /* Call */5 :
            var match = $$eval(expr._0, env);
            if (match.TAG === /* Vint */0) {
              throw {
                    RE_EXN_ID: "Match_failure",
                    _1: [
                      "Lab.res",
                      154,
                      14
                    ],
                    Error: new Error()
                  };
            }
            var args_value = Belt_List.map(expr._1, (function(env){
                return function (arg) {
                  return $$eval(arg, env);
                }
                }(env)));
            var fun_env = Belt_List.concat(args_value, match._0);
            _env = fun_env;
            _expr = match._1;
            continue ;
        case /* Let */6 :
            _env = {
              hd: $$eval(expr._0, env),
              tl: env
            };
            _expr = expr._1;
            continue ;
        
      }
    };
  };
  var vint = $$eval(expr, /* [] */0);
  if (vint.TAG === /* Vint */0) {
    return vint._0;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Lab.res",
          167,
          13
        ],
        Error: new Error()
      };
}

var Ir1 = {
  vadd: vadd$1,
  vmul: vmul$1,
  interpret: interpret$1
};

function index(l, s) {
  if (l) {
    if (Caml_obj.equal(l.hd, s)) {
      return 0;
    } else {
      return 1 + index(l.tl, s) | 0;
    }
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Lab.res",
          186,
          11
        ],
        Error: new Error()
      };
}

function convert(expr) {
  var compile = function (expr, env) {
    switch (expr.TAG | 0) {
      case /* Cst */0 :
          return {
                  TAG: /* Cst */0,
                  _0: expr._0
                };
      case /* Var */1 :
          return {
                  TAG: /* Var */1,
                  _0: index(env, expr._0)
                };
      case /* Add */2 :
          return {
                  TAG: /* Add */2,
                  _0: compile(expr._0, env),
                  _1: compile(expr._1, env)
                };
      case /* Mul */3 :
          return {
                  TAG: /* Mul */3,
                  _0: compile(expr._0, env),
                  _1: compile(expr._1, env)
                };
      case /* Fn */4 :
          return {
                  TAG: /* Fn */4,
                  _0: compile(expr._1, Belt_List.concat(expr._0, env))
                };
      case /* Call */5 :
          return {
                  TAG: /* Call */5,
                  _0: compile(expr._0, env),
                  _1: List.map((function (ex) {
                          return compile(ex, env);
                        }), expr._1)
                };
      case /* Let */6 :
          return {
                  TAG: /* Let */6,
                  _0: compile(expr._1, env),
                  _1: compile(expr._2, {
                        hd: expr._0,
                        tl: env
                      })
                };
      
    }
  };
  return compile(expr, /* [] */0);
}

var Compiler = {
  index: index,
  convert: convert
};

var exprIr0 = {
  TAG: /* Call */5,
  _0: {
    TAG: /* Fn */4,
    _0: {
      hd: "x",
      tl: /* [] */0
    },
    _1: {
      TAG: /* Add */2,
      _0: {
        TAG: /* Var */1,
        _0: "x"
      },
      _1: {
        TAG: /* Var */1,
        _0: "x"
      }
    }
  },
  _1: {
    hd: {
      TAG: /* Cst */0,
      _0: 1
    },
    tl: /* [] */0
  }
};

var result = interpret(exprIr0);

console.log(result);

var exprIr1 = {
  TAG: /* Call */5,
  _0: {
    TAG: /* Fn */4,
    _0: {
      TAG: /* Add */2,
      _0: {
        TAG: /* Var */1,
        _0: 0
      },
      _1: {
        TAG: /* Var */1,
        _0: 0
      }
    }
  },
  _1: {
    hd: {
      TAG: /* Cst */0,
      _0: 1
    },
    tl: /* [] */0
  }
};

var result$1 = interpret$1(exprIr1);

console.log(result$1);

if (!Caml_obj.equal(convert(exprIr0), exprIr1)) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Lab.res",
          217,
          0
        ],
        Error: new Error()
      };
}

export {
  Ir0 ,
  Ir1 ,
  Compiler ,
  exprIr0 ,
  exprIr1 ,
  result$1 as result,
}
/* result Not a pure module */
