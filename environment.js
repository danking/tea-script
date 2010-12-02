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


  function foldl (procedure, base, list) {
    var result = base;
    for(var i = 0; i < list.length; i++) {
      result = procedure(list[i], result);
    }
    return result;
  }

  function foldr (procedure, base, list) {
    var result = base;
    for(var i = list.length; i > 0; i--) {
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

  toplevel.foldl = foldl;
  toplevel.foldr = foldr;
  toplevel.map = map;
  toplevel.filter = filter;
  toplevel.andmap = andmap;
  toplevel.ormap = ormap;

  toplevel.__tea_quote = __tea_quote;
  toplevel.symbolp = symbolp0;
  toplevel.symbolep = symbolep0;
}
