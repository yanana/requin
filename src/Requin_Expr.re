module Js' = Js;

open Relude;

open Relude.Function;

open Relude.Function.Infix;

module Term = Requin_Term;
module Expression = Requin_Expression;

module E =
  Expression.Make({
    type nonrec t = Term.t;
    let compare = Term.cmp;
  });

type t = E.T.t;

let rec eq: (t, t) => bool =
  (a, b) =>
    switch (a, b) {
    | (`Literal(e), `Literal(e')) => Term.eq(e, e')
    | (`And(es), `And(es'))
    | (`Or(es), `Or(es')) => Belt.Array.eq(es |> Set.toArray, es' |> Set.toArray, eq)
    | _ => false
    };

let rec toString = (~length=?) =>
  fun
  | `Literal(t) => t |> Term.toString(~length?)
  | `And(es) => es |> Set.toArray |> Array.map(toString(~length?)) |> Js'.Array.joinWith(" * ")
  | `Or(es) => es |> Set.toArray |> Array.map(toString(~length?)) |> Js'.Array.joinWith(" + ");

module Literal = {
  let zero = `Literal(Term.fromS("0"));
};

module And = {
  let from = es => {
    let es' =
      es
      |> List.foldLeft(
           acc =>
             fun
             | `And(es') => acc |> Set.union(es')
             | e => acc |> Set.add(e),
           E.S.empty,
         );

    if (Set.length(es') == 1) {
      (es' |> Set.toArray)->Belt.Array.getUnsafe(0);
    } else {
      `And(es');
    };
  };

  let of2 =
    curry2(
      fun
      | (`And(es), `And(es')) => es |> Set.union(es') |> Set.toList |> from
      | (`And(es), e) => es |> Set.add(e) |> Set.toList |> from
      | (a, b) => E.S.empty |> Set.add(a) |> Set.add(b) |> Set.toList |> from,
    );
};

module Or = {
  let from = es => {
    let es' =
      es
      |> List.foldLeft(
           acc =>
             fun
             | `Or(es') => acc |> Set.union(es')
             | e => Set.add(e, acc),
           E.S.empty,
         );
    if (Set.length(es') == 1) {
      (es' |> Set.toArray)->Belt.Array.getUnsafe(0);
    } else {
      `Or(es');
    };
  };

  let of2 =
    curry2(
      fun
      | (`Or(es), `Or(es')) => es |> Set.union(es') |> Set.toList |> from
      | (`Or(es), e)
      | (e, `Or(es)) => es |> Set.add(e) |> Set.toList |> from
      | (a, b) => (E.S.empty |> Set.add(a) |> Set.add(b))->`Or,
    );
};

let literal = t => `Literal(t);

let (+++): (t, t) => t =
  (a, b) =>
    switch (a, b) {
    | (`Literal(_) as a, b)
    | (`And(_) as a, b)
    | (`Or(_) as a, `Literal(_) as b)
    | (`Or(_) as a, `And(_) as b) => Or.of2(a, b)
    | (`Or(es), `Or(es')) => (es |> Set.union(es'))->`Or
    };

let rec ( *** ): (t, t) => t =
  (a, b) =>
    switch (a, b) {
    | (`Literal(_) as a, `Literal(_) as b) => And.of2(a, b)
    | (`Literal(_) as a, `And(es)) => (es |> Set.add(a))->`And
    | (`Literal(_) as a, `Or(es)) => `Or(es |> Set.toArray |> Array.map(e => a *** e) |> Set.fromArray(E.S.id))
    | (`And(es), `Literal(_) as e) => `And(es |> Set.add(e))
    | (`And(es), `And(es')) => `And(es |> Set.union(es'))
    | (`And(_) as a, `Or(es')) => Or.from(es' |> Set.toList |> List.map(( *** )(a)))
    | (`Or(es), `Literal(_) as e) => (es |> Set.toList |> List.map(e' => e *** e') |> Set.fromList(E.S.id))->`Or
    | (`Or(es), `Or(es')) =>
      let cartesian = List.apply << List.map(( *** ));
      (cartesian(es |> Set.toList, es' |> Set.toList) |> Set.fromList(E.S.id))->`Or;
    | (`Or(es), `And(_) as a) => (es |> Set.toList |> List.map(e => e *** a) |> Set.fromList(E.S.id))->`Or
    };

let rec toDnf =
  fun
  | `Literal(_) as l => l
  | `And(es) =>
    es
    |> Set.toList
    |> List.map(toDnf)
    |> (
      fun
      | [] => Literal.zero
      | [x] => x
      | [head, ...rest] => rest |> List.foldLeft(( *** ), head)
    )
  | e => e;
