open Relude;

type t('a, 'identity) = [
  | `And(Set.t(t('a, 'identity), 'identity))
  | `Or(Set.t(t('a, 'identity), 'identity))
  | `Literal('a)
];

module type Comparable = {
  type t;
  let compare: (t, t) => int;
};

module type E = {
  type e;
  module rec S: {
    module rec C: Belt.Id.Comparable with type t = T.t;
    let id: Belt.Id.comparable(C.t, C.identity);
    type t = Belt.Set.t(C.t, C.identity);
    let empty: t;
    let fromList: list(C.t) => t;
  }
  and T: {type nonrec t = t(e, S.C.identity);};
};

module Make = (CA: Comparable) : (E with type e = CA.t) => {
  // type t = CA.t;
  type e = CA.t;

  module rec S: {
    module rec C: Belt.Id.Comparable with type t = T.t; // expr(CA.t, C.identity);
    let id: Belt.Id.comparable(C.t, C.identity);
    type t = Belt.Set.t(C.t, C.identity);
    let empty: t; // Belt.Set.t(expr(t, C.identity), C.identity);
    let fromList: list(C.t) => t;
  } = {
    module rec C: Belt.Id.Comparable with type t = t(CA.t, C.identity) =
      Belt.Id.MakeComparable({
        type nonrec t = t(CA.t, C.identity);
        // type t = T.t;
        let rec cmp = (a, b) => {
          let cmp' = (es, es') =>
            es
            |> Set.toArray
            |> Array.zip(es' |> Set.toArray)
            |> Array.map(((a, b)) => cmp(a, b))
            |> Array.dropWhile((==)(0))
            |> Array.head
            |> Option.getOrElse(0);
          switch (a, b) {
          | (`Literal(id), `Literal(id')) => CA.compare(id, id')
          | (`Literal(_), _) => (-1)
          | (`And(es), `And(es')) =>
            if (Set.eq(es, es')) {
              0;
            } else {
              switch (Set.(length(es), length(es'))) {
              | (s, s') when s == s' => cmp'(es, es')
              | (s, s') when s < s' => (-1)
              | _ => 1
              };
            }
          | (`And(_), `Literal(_)) => 1
          | (`And(_), _) => (-1)
          | (`Or(es), `Or(es')) =>
            Set.eq(es, es')
              ? 0
              : (
                switch (Set.(length(es), length(es'))) {
                | (s, s') when s == s' => cmp'(es, es')
                | (s, s') when s < s' => (-1)
                | _ => 1
                }
              )
          | (`Or(_), `Literal(_)) => 1
          | (`Or(_), _) => (-1)
          };
        };
      });

    type t = Belt.Set.t(C.t, C.identity);
    let id: Belt.Id.comparable(C.t, C.identity) = (module C);
    let empty: t = Set.empty((module C));
    let fromList: list(C.t) => t = Set.fromList((module C));
    let map = (f, identity, es) => es |> Set.toArray |> Array.map(f) |> Set.fromArray(identity);
  }
  and T: {type nonrec t = t(e, S.C.identity);} = T;
};
