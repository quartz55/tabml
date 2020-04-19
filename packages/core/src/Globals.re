include Relude.Globals;
include BsWebext.Globals;

let stringifyExn = v => Js.Json.stringifyAny(v) |> Option.getOrThrow;

module Logger = {
  let level = 3;
  type msgf('a) =
    (format4('a, Format.formatter, unit, string) => 'a) => string;

  let _base: msgf('a) => string =
    msgf => msgf(fmt => Format.asprintf(fmt ^^ "@."));

  let trace = k => level > 4 ? _base(k) |> Js.Console.log : ();
  let debug = k => level > 3 ? _base(k) |> Js.Console.log : ();
  let log = debug;
  let info = k => level > 2 ? _base(k) |> Js.Console.info : ();
  let warn = k => level > 1 ? _base(k) |> Js.Console.warn : ();
  let err = k => level > 0 ? _base(k) |> Js.Console.error : ();
};