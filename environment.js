function EnvironmentModule(toplevel) {

  // +
  function a0 () {
    return foldl(function (x, y) {
                   return x + y;
                 },
                 0,
                 arguments_to_array(arguments));
  }

  // -
  function _0 () {
    var args = arguments_to_array(arguments);
    return args.length === 1 ? -args :
      args[0] - a0.apply(this, args.slice(1));
  }

  // *
  function m0 () {
    return foldl(function(x, y) {
                   return x * y;
                 },
                 1,
                 arguments_to_array(arguments));
  }

  // /
  function d0 () {
    var args = arguments_to_array(arguments);
    return args.length === 1 ? 1/args :
      args[0] / m0.apply(this, args.slice(1));
  }

  function add1(val) {
    return val + 1;
  }

  function sub1(val) {
    return val - 1;
  }

  function sqr(val) {
    return val * val;
  }

  // <=
  function le0 () {
    return generic_numeric_comparison_function("<=",
                                               function(a,b) {
                                                 return a <= b;
                                               },
                                               arguments);
  }

  // <
  function l0 () {
    return generic_numeric_comparison_function("<",
                                              function(a,b) {
                                                return a < b;
                                              },
                                              arguments);
  }

  // >=
  function ge0 () {
    return generic_numeric_comparison_function(">=",
                                              function(a,b) {
                                                return a >= b;
                                              },
                                              arguments);
  }

  // >
  function g0 () {
    return generic_numeric_comparison_function(">",
                                              function(a,b) {
                                                return a > b;
                                              },
                                              arguments);
  }

  // =
  function e0 () {
    return generic_numeric_comparison_function("=",
                                              function(a,b) {
                                                return a === b;
                                              },
                                              arguments);
  }

  function generic_numeric_comparison_function (name,
                                                compare,
                                                arguments_object) {
    var args = arguments_to_array(arguments_object);
    var length = args.length;
    if(length <= 1) {
      throw name + ": expects at least 2 arguments, given " +
        length +
        (length == 1 ? ": " + args[0] : "");
    } else {
      return short_circuiting_predicate(compare, args);
    }
  }

  // THIS DOESN'T ACTUALLY WORK
  // I need to delay (make lazy, put into a thunk, etc.) all the args to
  // all short circuiting predicates.  Alternatively, I could stop being a pussy
  // and just write the compiler to properly output squeneces of <= or ||
  function short_circuiting_predicate(predicate, args) {
    var previous = args[0];
    for(var i = 1; i < args.length; i++) {
      if(!predicate(previous, args[i])) {
        return false; // short circuit
      }
    }
    return true; // no short circuit, all true
  }

  function cons (left, right) {
    return [left].concat(right);
  }

  function first (array) {
    return array[1];
  }

  function rest (array) {
    return array.slice(1);
  }

  function foldl (procedure, base, list) {
    var result = base;
    for(var i = 0; i < list.length; i++) {
      result = procedure(list[i], result);
    }
    return result;
  }

  function foldr (procedure, base, list) {
    var result = base;
    for(var i = list.length - 1; i >= 0; i--) {
      result = procedure(list[i], result);
    }
    return result;
  }

  function map (procedure, list) {
    return list.map(procedure);
  }

  function filter (procedure, list) {
    return list.filter(procedure);
  }

  function andmap (procedure, list) {
    return list.every(procedure);
  }

  function ormap (procedure, list) {
    return list.some(procedure);
  }

  function build_list0(n, proc) {
    var result = new Array(n);
    for(var i=0; i < n; i++) {
      result[i] = proc(i);
    }
  }

  function __tea_quote(value) {
    this.value = value;
  }

  function symbolp0(value) {
    return value instanceof __tea_quote;
  }

  function symbolep0(v1, v2) {
    return v1.value === v2.value;
  }

  // utility functions

  function arguments_to_array(args) {
    return Array.prototype.slice.call(args, 0);
  }

  // (provide ...)
  toplevel.a0 = a0;
  toplevel._0 = _0;
  toplevel.m0 = m0;
  toplevel.d0 = d0;
  toplevel.sub1 = sub1;
  toplevel.add1 = add1;
  toplevel.sqr = sqr;

  toplevel.le0 = le0;
  toplevel.l0 = l0;
  toplevel.ge0 = ge0;
  toplevel.g0 = g0;
  toplevel.e0 = e0;

  toplevel.cons = cons;
  toplevel.first = first;
  toplevel.rest = rest;

  toplevel.foldl = foldl;
  toplevel.foldr = foldr;
  toplevel.map = map;
  toplevel.filter = filter;
  toplevel.andmap = andmap;
  toplevel.ormap = ormap;

  toplevel.__tea_quote = __tea_quote;
  toplevel.symbolp0 = symbolp0;
  toplevel.symbolep0 = symbolep0;
}
