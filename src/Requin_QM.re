module Js' = Js;
module Option = Relude.Option;
module List = Relude.List;
module Map = Relude.Map;
module Array = Relude.Array;
module Set = Relude.Set;
module Tuple = Relude.Tuple;

open Relude.Function;
open Relude.Function.Infix;

module Term = Requin_Term;
module Bool = Requin_Bool;

module WM = {
  module C =
    Belt.Id.MakeComparable({
      type t = int;
      let cmp = compare;
    });

  type v = list(Term.t);
  type t = Belt.Map.t(C.t, v, C.identity);
  let empty: t = Belt.Map.make(~id=(module C));

  let toString = (~length=?, wm) => {
    let buff = Buffer.create(256);
    buff->Buffer.add_char('{');
    let l = wm |> Map.length;
    (wm |> Map.toList)
    ->Belt.List.forEachWithIndex((i, (k, v)) => {
        let (k', v') = (
          k->string_of_int,
          "["
          ++ (
            v
            |> List.map(Term.toString(~length?))
            |> List.toArray
            |> Js'.Array.joinWith(", ")
          )
          ++ "]",
        );
        buff->Buffer.add_string(k');
        buff->Buffer.add_string(": ");
        buff->Buffer.add_string(v');
        if (i < l - 1) {
          buff->Buffer.add_string(", ");
        } else {
          ();
        };
      });
    buff->Buffer.add_char('}');
    buff->Buffer.contents;
  };
};

let groupByHammingWeight = terms =>
  terms |> Map.groupListBy((module WM.C), v => v |> Term.hammingWeight);

module Memo = {
  module C =
    Belt.Id.MakeComparable({
      type t = Term.t;
      let cmp = Term.cmp;
    });

  type v = bool;
  type t = Belt.Map.t(C.t, v, C.identity);
  let empty = Belt.Map.make(~id=(module C));

  let toString = (~length=?, m) => {
    let buff = Buffer.create(256);
    buff->Buffer.add_char('{');
    let l = m |> Map.length;
    (m |> Map.toList)
    ->Belt.List.forEachWithIndex((i, (k, v)) => {
        let (k', v') = (Term.toString(~length?, k), v->string_of_bool);
        buff->Buffer.add_string(k');
        buff->Buffer.add_string(": ");
        buff->Buffer.add_string(v');
        if (i < l - 1) {
          buff->Buffer.add_string(", ");
        } else {
          ();
        };
      });
    buff->Buffer.add_char('}');
    buff->Buffer.contents;
  };
};

module rec T2T: {
  module C = Term.S.C;
  type k = Term.t;
  type v = Term.t;
  type t = Belt.Map.t(C.t, v, C.identity);
  let empty: t;
  let fromList: list((k, v)) => t;
  let eq: (t, t) => bool;
  let show: t => string;
} = {
  module C = Term.S.C;
  type k = Term.t;
  type v = Term.t;
  type t = Belt.Map.t(C.t, v, C.identity);
  let empty = Belt.Map.make(~id=(module T2T.C));
  let fromList: list((k, v)) => t = Map.fromList((module C));
  let eq: (t, t) => bool = Map.eqBy(Term.eq);
  let show: t => string =
    Map.toList >> List.showBy(Tuple.showBy2(Term.toString, Term.toString));
}
and PIT: {
  module C = Term.S.C;
  type v = Term.S.t;
  type t = Belt.Map.t(C.t, v, C.identity);
  let empty: t;
  let eq: (t, t) => bool;
  let make: (list(Term.t), Term.S.t) => t;
  let removeDominatingRows: t => t;
  let removeDominatingColumns: t => t;
  let removePrimaryEssentialPrimeImplicants: (T2T.t, t) => t;
  let reduce: t => (list(Term.t), t);
  let findEssentialPrimeImplicants: t => T2T.t;
  let minimizeExpression: t => option(Bool.t);
  let extractShortestTerms: Bool.t => Term.S.t;
  let petrick: t => Term.S.t;
  let fromList: list((Term.t, list(Term.t))) => t;
  let show: t => string;
  let (|=|): (t, t) => bool;
  let (|!=|): (t, t) => bool;
  module Eq: BsAbstract.Interface.EQ with type t = t;
} = {
  module C = Term.S.C;
  type v = Term.S.t;
  type t = Belt.Map.t(C.t, v, C.identity);
  let empty = Belt.Map.make(~id=(module PIT.C));

  let eq = Map.eqBy(Term.S.eq);

  module Eq: BsAbstract.Interface.EQ with type t = t = {
    type nonrec t = t;
    let eq = eq;
  };

  include Relude_Extensions_Eq.EqExtensions(Eq);
  include Relude_Extensions_Eq.EqInfix(Eq);

  let make: (list(Term.t), Term.S.t) => PIT.t =
    (minterms, implicants) =>
      minterms
      |> List.foldLeft(
           (acc, Term.Term(v, _) as minterm) => {
             let coveringTerms = implicants |> Set.filter(Term.covers(v));
             acc |> Map.set(minterm, coveringTerms);
           },
           empty,
         );

  let fromList: list((Term.t, list(Term.t))) => t =
    List.map(((t, terms)) => (t, terms |> Set.fromList((module C))))
    >> Map.fromList((module C));

  let show: t => string =
    Map.toList >> List.showBy(Tuple.showBy2(Term.show, Term.S.show));

  let findEssentialPrimeImplicants =
    Map.foldLeft(
      (acc, minterm) =>
        Set.toList
        >> (
          fun
          | [term] => acc |> Map.set(minterm, term)
          | _ => acc
        ),
      T2T.empty,
    );

  let removeDominatingRows: t => t =
    pit =>
      pit
      |> Map.foldLeft(
           (acc, minterm, implicants) => {
             let dominatees =
               pit
               |> Map.foldLeft(
                    (acc, m, candidates) =>
                      if (Term.(m |=| minterm)) {
                        acc;
                      } else {
                        candidates |> Set.all(flip(Set.contains, implicants))
                          ? Set.add(m, acc) : acc;
                      },
                    Term.S.empty,
                  );
             Set.isEmpty(dominatees)
               ? acc : acc |> Map.set(minterm, dominatees);
           },
           empty,
         )
      |> Map.foldLeft(
           (acc, minterm, _) =>
             acc
             |> Map.remove(minterm)
             |> Map.any((_, v) => Set.contains(minterm, v))
               ? acc : acc |> Map.remove(minterm),
           pit,
         );

  let transpose: t => t =
    table =>
      table
      |> Map.foldLeft(
           (acc, k, terms) =>
             terms
             |> Set.foldLeft(
                  (acc, t) =>
                    acc
                    |> Map.set(
                         t,
                         acc |> Map.getOrElse(t, Term.S.empty) |> Set.add(k),
                       ),
                  acc,
                ),
           empty,
         );

  let removePrimaryEssentialPrimeImplicants: (T2T.t, t) => t =
    (essentialPrimeImplicants, pit) => {
      let pit' =
        essentialPrimeImplicants
        |> Map.keys
        |> List.foldLeft(flip(Map.remove), pit);
      essentialPrimeImplicants
      |> Map.values
      |> List.foldLeft(
           (acc, epi) =>
             acc
             |> Map.foldLeft(
                  (acc, minterm, pis) =>
                    pis |> Set.contains(epi)
                      ? acc |> Map.remove(minterm) : acc,
                  acc,
                ),
           pit',
         );
    };

  let removeDominatingColumns: t => t =
    pit => {
      let transposed = pit |> transpose;
      let dominance: t =
        transposed
        |> Map.foldLeft(
             (acc, pi, minterms) => {
               let dominatees: Term.S.t =
                 transposed
                 |> Map.foldLeft(
                      (acc, i, candidates) =>
                        Term.(pi |!=| i) && Set.subset(candidates, minterms)
                          ? Set.add(i, acc) : acc,
                      Term.S.empty,
                    );

               dominatees |> Set.isEmpty
                 ? acc : acc |> Map.set(pi, dominatees);
             },
             empty,
           );

      let removeCodominance: t => t =
        dominance => {
          let delibles: Term.S.t =
            dominance
            |> Map.foldLeft(
                 (acc, pi, _) =>
                   if (acc |> Set.contains(pi)) {
                     acc;
                   } else {
                     dominance
                     |> Map.remove(pi)
                     |> Map.filter((k, _) => !Set.contains(k, acc))
                     |> Map.filter((_, v) => v |> Set.contains(pi))
                     |> Map.keys
                     |> Set.fromList((module Term.S.C))
                     |> Set.union(acc);
                   },
                 Term.S.empty,
               );

          dominance
          |> Map.filter((term, _) => delibles |> Set.contains(term));
        };

      let delibles =
        dominance
        |> removeCodominance
        |> Map.values
        |> List.foldLeft(Set.union, Term.S.empty);

      pit |> Map.map(terms => Set.diff(terms, delibles));
    };

  let reduce: t => (list(Term.t), t) =
    table => {
      let rec loop = (epis, table) =>
        if (Map.isEmpty(table)) {
          (epis, table);
        } else {
          let epis' =
            Map.merge(
              _ =>
                curry2(
                  fun
                  | (Some(_) as a, _) => a
                  | (_, Some(_) as b) => b
                  | _ => None,
                ),
              epis,
              findEssentialPrimeImplicants(table),
            );

          let table' =
            table
            |> (
              removePrimaryEssentialPrimeImplicants(epis')
              >> removeDominatingRows
              >> removeDominatingColumns
            );
          table |=| table' ? (epis', table) : loop(epis', table');
        };

      loop(T2T.empty, table) |> (((epis, t)) => (Map.values(epis), t));
    };

  let minimizeExpression: t => option(Bool.t) =
    table => {
      table
      |> Map.values
      |> List.map(Set.toArray >> Array.map(Term.show >> Bool.literal))
      |> List.foldRight(
           (curr, acc) =>
             switch (curr) {
             | [||] => acc
             | [|l|] => [l, ...acc]
             | ls => [Bool.Or.from(ls), ...acc]
             },
           [],
         )
      |> (
        fun
        | [] => None
        | [head, ...rest] => Some(List.foldLeft(Bool.( *** ), head, rest))
        // | [head, ...rest] => Some(List.foldWith)
      );
    };

  let extractShortestTerms: Bool.t => Term.S.t =
    e => {
      let rec genTerms: Bool.t => list(Term.S.t) =
        fun
        | `Zero => [Term.S.singleton(Term.zero)]
        | `One => [Term.S.singleton(Term.one)]
        // A: only a variable.
        | `Literal(id) => [Term.S.singleton(Term.fromS(id))]
        // ABC: only a product.
        | `And(terms) =>
          terms
          |> Set.foldLeft(
               acc =>
                 fun
                 | `Literal(id) => acc |> Set.add(Term.fromS(id))
                 | _ => acc,
               Term.S.empty,
             )
          |> List.pure
        // AB + BC + D
        | `Or(terms) =>
          terms
          |> Set.foldLeft(
               acc =>
                 fun
                 | `Or(_) => acc
                 | t => acc |> List.concat(genTerms(t)),
               [],
             );

      genTerms(e)
      |> (
        fun
        | [] => Term.S.empty
        | [head, ...tail] =>
          tail
          |> List.foldLeft(
               (acc, terms) =>
                 Set.length(terms) <= Set.length(acc) ? terms : acc,
               head,
             )
      );
    };

  let petrick: t => Term.S.t = {
    minimizeExpression
    >> Option.map(extractShortestTerms)
    >> Option.getOrElse(Term.S.empty);
  };
};

let combine: WM.t => Memo.t =
  weightMap => {
    let rec loop: (WM.t, Memo.t) => Memo.t =
      (wm, memo) =>
        if (wm |> Map.values |> List.flatten |> List.isEmpty) {
          memo;
        } else {
          let memo' =
            wm
            |> Map.values
            |> List.foldLeft(
                 (acc, terms) =>
                   terms
                   |> List.foldLeft(
                        (acc, t) => acc |> Map.set(t, false),
                        acc,
                      ),
                 memo,
               );
          let (combined, m') =
            wm
            |> Map.toList
            |> List.foldLeft(
                 ((wm', memo), (weight, terms)) => {
                   let candidates =
                     wm |> Map.get(weight + 1) |> Option.getOrElse([]);
                   let combine' = (term, (terms, memo) as acc, candidate) =>
                     Term.combine(term, candidate)
                     |> Option.fold(acc, combined =>
                          (
                            [combined, ...terms],
                            memo
                            |> Map.set(term, true)
                            |> Map.set(candidate, true),
                          )
                        );
                   let (combined, m) =
                     terms
                     |> List.foldLeft(
                          (acc, term) =>
                            candidates |> List.foldLeft(combine'(term), acc),
                          ([], memo),
                        );
                   (
                     wm'
                     |> Map.update(
                          weight,
                          Option.map(List.concat(combined))
                          >> Option.orElse(~fallback=Some(combined)),
                        ),
                     m,
                   );
                 },
                 (WM.empty, memo'),
               );
          loop(combined, m');
        };
    loop(weightMap, Memo.empty);
  };

let createImplicants: Memo.t => Term.S.t =
  Map.foldLeft(
    (acc, term, checked) => checked ? acc : acc |> Set.add(term),
    Term.S.empty,
  );

let solve: list(int) => Term.S.t =
  List.distinct((module Relude.Int.Eq))
  >> List.map((land)(255) >> Term.ofN)
  >> (
    fun
    | [] => Term.S.empty
    | [t] => Term.S.singleton(t)
    | terms =>
      terms
      |> groupByHammingWeight
      |> combine
      |> createImplicants
      |> PIT.make(terms)
      |> PIT.reduce
      |> (
        ((epis, table)) =>
          (epis |> Term.S.fromList, PIT.petrick(table))
          |> uncurry2(Set.union)
      )
  );
