module Js' = Js;

module String' = String;

open Relude;

module Expression = Requin_Expression;

module E =
  Expression.Make({
    type t = string;
    let compare = String'.compare;
  });

module S = E.S;

type t = E.T.t;

let map = (s, f) => s |> Set.toArray |> Array.map(f) |> Set.fromArray(S.id);

let eq = (a, b) =>
  switch (a, b) {
  | (`Zero, `Zero) => true
  | (`One, `One) => true
  | (`Literal(id), `Literal(id')) => String.eq(id, id')
  | (`And(es), `And(es'))
  | (`Or(es), `Or(es')) => Set.eq(es, es')
  | (`Literal(_), _)
  | (`And(_), _)
  | (`Or(_), _) => false
  | _ => false
  };

module Eq: BsAbstract.Interface.EQ with type t = t = {
  type nonrec t = t;
  let eq = eq;
};

include Relude_Extensions_Eq.EqExtensions(Eq);
include Relude_Extensions_Eq.EqInfix(Eq);

let rec toString: t => string =
  fun
  | `Zero => "0"
  | `One => "1"
  | `Literal(id) => id
  | `And(es) => {
      let s =
        es
        |> Set.toArray
        |> Array.map(toString)
        |> Js'.Array.joinWith(" AND ");
      "(" ++ s ++ ")";
    }
  | `Or(es) =>
    es
    |> Set.toArray
    |> Array.map(toString)
    |> Js'.Array.joinWith(" OR ")
    |> (s => "(" ++ s ++ ")");

let show: t => string = toString;

module Show: BsAbstract.Interface.SHOW with type t = t = {
  type nonrec t = t;
  let show = show;
};

// Minimize X + XY => X
let minimize = es =>
  es
  |> Set.foldLeft(
       (acc, e) => {
         let delibles =
           switch (e) {
           | `One => acc |> Set.remove(e)
           | `Literal(_) =>
             acc
             |> Set.remove(e)
             |> Set.filter(
                  fun
                  | `And(es) => es |> Set.contains(e)
                  | _ => false,
                )
           | `And(es) =>
             acc
             |> Set.remove(e)
             |> Set.filter(
                  fun
                  | `And(es') => Set.subset(es, es')
                  | _ => false,
                )
           | _ => acc
           };
         Set.diff(acc, delibles);
       },
       es,
     );

module Or = {
  let from = es => (es |> Set.fromArray(S.id) |> minimize)->`Or;
};

module And = {
  let from = es => (es |> Set.fromArray(S.id))->`And;
};

let literal = id => `Literal(id);

let rec ( *** ) = (expr, expr') =>
  switch (expr, expr') {
  | (`Zero, _) => `Zero
  | (_, `Zero) => `Zero
  | (`One, e) => e
  | (e, `One) => e
  | (`Literal(_) as l, `Literal(_) as l') =>
    l |=| l' ? l : And.from([|l, l'|])
  | (`Literal(_) as l, `And(es)) => (es |> Set.add(l))->`And
  | (`Literal(_) as l, `Or(es)) => es->(map(e => l *** e))->`Or
  | (`And(es), `Literal(_) as l) => (es |> Set.add(l))->`And
  | (`And(es), `And(es')) => (es |> Set.union(es'))->`And
  | (`And(_) as a, `Or(es')) => `Or(es'->map(e => a *** e))
  | (`Or(es), _ as expr) =>
    let exprs =
      es->(map(e => e *** expr))
      |> Set.foldLeft(
           acc =>
             fun
             | `Or(es) => acc |> Set.union(es)
             | e => acc |> Set.add(e),
           S.empty,
         );
    Set.length(exprs) == 1
      ? exprs->Belt.Set.toArray->Belt.Array.getUnsafe(0)
      : `Or(minimize(exprs));
  };
