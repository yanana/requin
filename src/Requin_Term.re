module Js' = Js;

module Option = Relude.Option;
module List = Relude.List;
module Set = Relude.Set;

open Relude.Function;
open Relude.Function.Infix;

module Utils = Requin_Utils;

[@genType]
type t =
  | Term(int, int);

let zero = Term(0, 0);
let one = Term(-1, 0);

let eq = (a, b) =>
  switch (a, b) {
  | (Term(v, m), Term(v', m')) => v == v' && m == m'
  };

module Eq: BsAbstract.Interface.EQ with type t = t = {
  type nonrec t = t;
  let eq = eq;
};

include Relude_Extensions_Eq.EqExtensions(Eq);
include Relude_Extensions_Eq.EqInfix(Eq);

let cmp: (t, t) => int =
  curry2(
    fun
    | (Term(v, m), Term(v', m')) =>
      compare(v, v')
      |> (
        fun
        | 0 => compare(m, m')
        | n => n
      ),
  );

let toString = (~length=?, e) =>
  switch (e) {
  | Term(value, mask) =>
    let v = Js'.Int.toStringWithRadix(value, ~radix=2);
    let m = Js'.Int.toStringWithRadix(mask, ~radix=2);
    let l =
      length |> Option.getOrElse(max(String.length(v), String.length(m)));
    let (v', m') = Utils.(padStart(l, '0', v), padStart(l, '0', m));
    v' |> String.mapi((i, c) => m'.[i] == '1' ? '-' : c);
  };

let show: t => string = toString;

let toString' =
  fun
  | Term(v, m) =>
    "Term(" ++ string_of_int(v) ++ ", " ++ string_of_int(m) ++ ")";

module Show: BsAbstract.Interface.SHOW with type t = t = {
  type nonrec t = t;
  let show = show;
};

module S = {
  module C =
    Belt.Id.MakeComparable({
      type nonrec t = t;
      let cmp = cmp;
    });

  type t = Belt.Set.t(C.t, C.identity);

  let empty = Belt.Set.make(~id=(module C));

  let singleton = v => empty |> Set.add(v);

  let eq: (t, t) => bool =
    (a, b) =>
      (Set.toList(a), Set.toList(b)) |> uncurry2(List.eq((module Eq)));

  module Eq: BsAbstract.Interface.EQ with type t = t = {
    type nonrec t = t;
    let eq = eq;
  };

  include Relude_Extensions_Eq.EqExtensions(Eq);
  include Relude_Extensions_Eq.EqInfix(Eq);

  let fromList: list(C.t) => t = Set.fromList((module C));

  let show: t => string = Set.toList >> List.show((module Show));

  module Show:
    BsAbstract.Interface.SHOW with type t = Belt.Set.t(C.t, C.identity) = {
    type t = Belt.Set.t(C.t, C.identity);
    let show = show;
  };
};

let countSetBits = n => {
  let rec loop = (n, c) => n <= 0 ? c : loop(n land (n - 1), c + 1);
  loop(n, 0);
};

let error = Term(0, -1);

let _pattern = Js'.Re.fromString("^[01-]+$");

let fromS =
  fun
  | "" => error
  | s when !Js'.Re.test_(_pattern, s) => error
  | s => {
      let (_, (v, m)) =
        Utils.explode(s)
        |> List.foldLeft(
             ((l, (v, m)), c) => {
               switch (c) {
               | '-' => (l - 1, (v, m lor 1 lsl l))
               | '1' => (l - 1, (v lor 1 lsl l, m))
               | _ => (l - 1, (v, m))
               }
             },
             (String.length(s) - 1, (0, 0)),
           );
      Term(v, m);
    };

let fromN = (v, m) => {
  let mask = v lxor m;
  let value = v land m land lnot(mask);

  Term(value, mask);
};

let ofN = v => Term(v, 0);

let hammingWeight =
  fun
  | Term(v, m) => countSetBits(v land lnot(m));

let hammingDistance: (t, t) => int =
  curry2(
    fun
    | (Term(v, m), Term(v', m')) => {
        let hd = (a, b) => countSetBits(a lxor b);
        hd(v, v') + hd(m, m');
      },
  );

let isCombinable = (a, b) => hammingDistance(a, b) == 1;

let combine = (a, b) =>
  if (!isCombinable(a, b)) {
    None;
  } else {
    switch (a, b) {
    | (Term(v, m), Term(v', m')) =>
      let mask = m lor m' lor v lxor v';
      let value = v land v' land lnot(m);
      Some(Term(value, mask));
    };
  };

let covers: (int, t) => bool =
  curry2(
    fun
    | (t, Term(v, m)) => v lxor t lor m == m,
  );

module T = {
  type t =
    | One(string)
    | Zero(string)
    | Ignore;

  let toString =
    fun
    | One(v) => {j|One($v)|j}
    | Zero(v) => {j|Zero($v)|j}
    | Ignore => "Ignore";

  let fromPair = (c, v) =>
    switch (c) {
    | '1' => One(v)
    | '0' => Zero(v)
    | _ => Ignore
    };

  let isOne =
    fun
    | One(_) => true
    | _ => false;

  let isIgnore =
    fun
    | Ignore => true
    | _ => false;
};

let reorder = terms =>
  terms
  |> Utils.findIndex(T.isOne)
  |> Option.map(firstTrueIndex => Utils.moveToFirst(firstTrueIndex, terms))
  |> Option.getOrElse(
       terms
       |> Utils.findIndex(T.isIgnore)
       |> Option.map(i => Utils.moveToFirst(i, terms))
       |> Option.getOrElse(terms),
     );

let toBooleanExpression: (list(string), t) => string =
  (variables, term) => {
    let s = term |> toString(~length=variables->Belt.List.length);
    let bits = s |> Utils.splitString;
    let terms =
      variables |> List.zip(bits) |> List.map(uncurry2(T.fromPair));

    reorder(terms)
    |> List.foldRight(
         (term, acc) =>
           switch (term) {
           | T.Ignore => acc
           | T.Zero(v) => [{j|NOT $v|j}, ...acc]
           | T.One(v) => [v, ...acc]
           },
         [],
       )
    |> List.toArray
    |> Js.Array.joinWith(" AND ");
  };
