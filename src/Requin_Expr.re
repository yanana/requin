module Js' = Js;

open Relude;

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
    | (`Zero, `Zero) => true
    | (`One, `One) => true
    | (`Literal(e), `Literal(e')) => Term.eq(e, e')
    | (`And(es), `And(es'))
    | (`Or(es), `Or(es')) =>
      Belt.Array.eq(es |> Set.toArray, es' |> Set.toArray, eq)
    | _ => false
    };

let rec toString = (~length=?) =>
  fun
  | `Zero => "0"
  | `One => "1"
  | `Literal(t) => t |> Term.toString(~length?)
  | `And(es) =>
    es
    |> Set.toArray
    |> Array.map(toString(~length?))
    |> Js'.Array.joinWith(" * ")
  | `Or(es) =>
    es
    |> Set.toArray
    |> Array.map(toString(~length?))
    |> Js'.Array.joinWith(" + ");

module Literal = {
  let zero = `Zero;
};

let literal = t => `Literal(t);

module Disjunctive = {
  let append: (t, t) => t =
    (a, b) =>
      switch (a, b) {
      | (`Zero, _) => b
      | (_, `Zero) => a
      | (`One, _)
      | (_, `One) => `One
      | (`Or(a), `Literal(_) as b) => `Or(a |> Set.add(b))
      | (`Or(a), `And(_) as b) => `Or(a |> Set.add(b))
      | (`Or(es), `Or(es')) => `Or(es |> Set.union(es'))
      | (a, `Or(es')) => `Or(es' |> Set.add(a))
      | (a, b) => `Or(E.S.empty |> Set.add(a) |> Set.add(b))
      };

  module Magma: BsAbstract.Interface.MAGMA with type t = t = {
    type nonrec t = t;
    let append = append;
  };
  module MedialMagma: BsAbstract.Interface.MEDIAL_MAGMA with type t = t = Magma;
  module Semigroup: BsAbstract.Interface.SEMIGROUP with type t = t = {
    include Magma;
  };
  include Relude_Extensions_Semigroup.SemigroupExtensions(Semigroup);
  include Relude_Extensions_Semigroup.SemigroupInfix(Semigroup);
  module Monoid: BsAbstract.Interface.MONOID with type t = t = {
    include Semigroup;
    let empty = `Zero;
  };
  include Relude_Extensions_Monoid.MonoidExtensions(Monoid);
  include Relude_List_Instances.FoldableMonoidExtensions(Monoid);

  let from = foldWithMonoid;
  let of2 = append;
};

module Or = Disjunctive;

module Conjunctive = {
  let rec append: (t, t) => t =
    (a, b) =>
      switch (a, b) {
      | (`Zero, _)
      | (_, `Zero) => `Zero
      | (`One, _) => b
      | (_, `One) => a
      | (`Literal(_) as a, `Literal(_) as b) =>
        `And(E.S.empty |> Set.add(a) |> Set.add(b))
      | (`Literal(_) as a, `Or(es)) =>
        `Or(es |> Set.toArray |> Array.map(append(a)) |> E.S.fromArray)
      | (`And(es), `Literal(_) as e)
      | (`Literal(_) as e, `And(es)) => `And(es |> Set.add(e))
      | (`And(es), `And(es')) => `And(es |> Set.union(es'))
      | (`And(_) as a, `Or(es')) =>
        Disjunctive.from(es' |> Set.toList |> List.map(append(a)))
      | (`Or(es), `Literal(_) as e) =>
        (es |> Set.toList |> List.map(append(e)) |> E.S.fromList)->`Or
      | (`Or(es), `Or(es')) =>
        let cartesian = List.apply << List.map(append);
        (cartesian(es |> Set.toList, es' |> Set.toList) |> E.S.fromList)->`Or;
      | (`Or(es), `And(_) as a) =>
        `Or(es |> Set.toArray |> Array.map(append(a)) |> E.S.fromArray)
      };

  module Magma: BsAbstract.Interface.MAGMA with type t = t = {
    type nonrec t = t;
    let append = append;
  };
  module MedialMagma: BsAbstract.Interface.MEDIAL_MAGMA with type t = t = Magma;
  module Semigroup: BsAbstract.Interface.SEMIGROUP with type t = t = {
    include Magma;
  };
  include Relude_Extensions_Semigroup.SemigroupExtensions(Semigroup);
  include Relude_Extensions_Semigroup.SemigroupInfix(Semigroup);
  module Monoid: BsAbstract.Interface.MONOID with type t = t = {
    include Semigroup;
    let empty = `One;
  };
  include Relude_Extensions_Monoid.MonoidExtensions(Monoid);
  include Relude_List_Instances.FoldableMonoidExtensions(Monoid);

  let from = foldWithMonoid;
  let of2 = append;
};

module And = Conjunctive;

module Semiring: BsAbstract.Interface.SEMIRING with type t = t = {
  type nonrec t = t;
  let zero = `Zero;
  let one = `One;
  let add = Disjunctive.append;
  let multiply = Conjunctive.append;
};

include BsAbstract.Infix.Semiring(Semiring);

let rec toDnf =
  fun
  | `Literal(_) as l => l
  | `And(es) =>
    es |> Set.toList |> List.map(toDnf) |> Conjunctive.foldWithMonoid
  | e => e;
