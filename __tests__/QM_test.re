module List = Relude.List;
module Map = Relude.Map;
module Array = Relude.Array;
module Option = Relude.Option;
open Relude.Function.Infix;

open Jest;
open Expect;

module QM = Requin_QM;
open QM;

module Bool = Requin_Bool;

describe("QM", () => {
  testAll(
    "groupByHammingWeight",
    [
      Term.(
        [fromS("1100"), fromS("1000"), fromS("0011"), fromS("0101")],
        WM.empty
        |> Map.set(1, [fromS("1000")])
        |> Map.set(2, [Term.fromS("1100"), fromS("0011"), fromS("0101")]),
      ),
    ],
    ((input, expected)) => {
      let eq' = (xs, ys) => Belt.List.eq(xs, ys, Term.eq);
      let actual = groupByHammingWeight(input);
      let result = Belt.Map.eq(actual, expected, eq');
      expect(result) |> toEqual(true);
    },
  );

  testAll(
    "combine",
    [
      (
        WM.empty
        |> Map.set(1, ["1", "010", "100"] |> List.map(v => Term.fromS(v)))
        |> Map.set(2, ["011", "101", "110"] |> List.map(v => Term.fromS(v))),
        Term.[
          (Term(1, 0), true),
          (Term(2, 0), true),
          (Term(3, 0), true),
          (Term(4, 0), true),
          (Term(5, 0), true),
          (Term(6, 0), true),
          (fromS("0-1"), false),
          (fromS("-01"), false),
          (fromS("01-"), false),
          (fromS("-10"), false),
          (fromS("10-"), false),
          (fromS("1-0"), false),
        ]
        |> Map.fromList((module Memo.C)),
      ),
      (
        Term.[(1, [Term(1, 0), Term(4, 0)]), (2, [Term(3, 0)])]
        |> Map.fromList((module WM.C)),
        [
          Term.(Term(1, 0), true),
          Term.(Term(4, 0), false),
          Term.(Term(3, 0), true),
          (
            Term.Term(1, 0)
            |> Term.combine(Term.Term(3, 0))
            |> Option.getOrElse(Term.error),
            false,
          ),
        ]
        |> Map.fromList((module Memo.C)),
      ),
    ],
    ((input: WM.t, expected: Memo.t)) => {
      let actual = combine(input);
      expect(actual |> Map.eqBy((==), expected)) |> toEqual(true);
    },
  );
});

describe("PIT", () => {
  testAll(
    "removeDominatingRows",
    [
      (
        [
          (Term.Term(1, 0), [Term.fromS("0-1"), Term.fromS("-01")]),
          (Term.Term(2, 0), [Term.fromS("01-"), Term.fromS("-10")]),
          (Term.Term(3, 0), [Term.fromS("0-1"), Term.fromS("01-")]),
          (Term.Term(4, 0), [Term.fromS("10-"), Term.fromS("1-0")]),
          (Term.Term(5, 0), [Term.fromS("-01"), Term.fromS("10-")]),
          (Term.Term(6, 0), [Term.fromS("-10"), Term.fromS("1-0")]),
        ]
        |> PIT.fromList,
        [
          (Term.Term(1, 0), [Term.fromS("0-1"), Term.fromS("-01")]),
          (Term.Term(2, 0), [Term.fromS("01-"), Term.fromS("-10")]),
          (Term.Term(3, 0), [Term.fromS("0-1"), Term.fromS("01-")]),
          (Term.Term(4, 0), [Term.fromS("10-"), Term.fromS("1-0")]),
          (Term.Term(5, 0), [Term.fromS("-01"), Term.fromS("10-")]),
          (Term.Term(6, 0), [Term.fromS("-10"), Term.fromS("1-0")]),
        ]
        |> PIT.fromList,
      ),
      //
    ],
    ((input, expected)) => {
      let actual = PIT.removeDominatingRows(input);
      expect(PIT.eq(actual, expected)) |> toEqual(true);
    },
  );

  testAll(
    "removeDominatingColumns",
    [
      (
        [
          (Term.fromS("0110"), [Term.fromS("-11-"), Term.fromS("--10")]),
          (Term.fromS("1100"), [Term.fromS("11--"), Term.fromS("1--0")]),
          //
        ]
        |> PIT.fromList,
        [
          (Term.fromS("0110"), [Term.fromS("-11-")]),
          (Term.fromS("1100"), [Term.fromS("11--")]),
          //
        ]
        |> PIT.fromList,
      ),
      (
        [
          (Term.Term(1, 0), [Term.fromS("0-1"), Term.fromS("-01")]),
          (Term.Term(2, 0), [Term.fromS("01-"), Term.fromS("-10")]),
          (Term.Term(3, 0), [Term.fromS("0-1"), Term.fromS("01-")]),
          (Term.Term(4, 0), [Term.fromS("10-"), Term.fromS("1-0")]),
          (Term.Term(5, 0), [Term.fromS("-01"), Term.fromS("10-")]),
          (Term.Term(6, 0), [Term.fromS("-10"), Term.fromS("1-0")]),
        ]
        |> PIT.fromList,
        [
          (Term.Term(1, 0), [Term.fromS("0-1"), Term.fromS("-01")]),
          (Term.Term(2, 0), [Term.fromS("01-"), Term.fromS("-10")]),
          (Term.Term(3, 0), [Term.fromS("0-1"), Term.fromS("01-")]),
          (Term.Term(4, 0), [Term.fromS("10-"), Term.fromS("1-0")]),
          (Term.Term(5, 0), [Term.fromS("-01"), Term.fromS("10-")]),
          (Term.Term(6, 0), [Term.fromS("-10"), Term.fromS("1-0")]),
        ]
        |> PIT.fromList,
      ),
    ],
    ((input, expected)) => {
      let actual = PIT.removeDominatingColumns(input);
      expect(PIT.eq(actual, expected)) |> toEqual(true);
    },
  );

  testAll("removePrimaryEssentialPrimeImplicants", [], _ => {
    expect(true) |> toEqual(true)
  });

  testAll(
    "findEssentialPrimeImplicants",
    [
      (
        [
          (1, ["0-1", "-01"]),
          (2, ["01-", "-10"]),
          (3, ["0-1", "01-"]),
          (4, ["10-", "1-0"]),
          (5, ["-01", "10-"]),
          (6, ["-10", "1-0"]),
        ]
        |> List.map(((i, ss)) =>
             (Term.Term(i, 0), List.map(Term.fromS, ss))
           )
        |> PIT.fromList,
        T2T.empty,
      ),
    ],
    ((input, expected)) => {
      let actual = PIT.findEssentialPrimeImplicants(input);
      expect(T2T.eq(actual, expected)) |> toEqual(true);
    },
  );

  (
    () => {
      let minterms =
        [
          (0, "0"),
          (2, "0010"),
          (8, "1000"),
          (5, "0101"),
          (6, "0110"),
          (10, "1010"),
          (12, "1100"),
          (7, "0111"),
          (13, "1101"),
          (14, "1110"),
          (15, "1111"),
        ]
        |> List.map(((i, s)) => (i, Term.fromS(s)))
        |> List.foldLeft(
             (acc, (i, t)) => Belt.Map.Int.set(acc, i, t),
             Belt.Map.Int.empty,
           );

      let implicants =
        (
          [|
            ("0_2_8_10", (0, 10)),
            ("2_6_10_14", (2, 12)),
            ("5_7_13_15", (5, 10)),
            ("6_7_14_15", (6, 9)),
            ("8_10_12_14", (8, 6)),
            ("12_13_14_15", (12, 3)),
          |]
          |> Belt.Map.String.fromArray
        )
        ->Belt.Map.String.map(((v, m)) => Term.Term(v, m));

      let get = Belt.Map.Int.getExn(minterms);

      let getI = Belt.Map.String.getExn(implicants);

      testAll(
        "reduce",
        [
          (
            [
              (0, ["0_2_8_10"]),
              (2, ["0_2_8_10", "2_6_10_14"]),
              (8, ["0_2_8_10", "8_10_12_14"]),
              (5, ["5_7_13_15"]),
              (6, ["2_6_10_14", "6_7_14_15"]),
              (10, ["0_2_8_10", "2_6_10_14"]),
              (12, ["8_10_12_14", "12_13_14_15"]),
              (7, ["5_7_13_15", "6_7_14_15"]),
              (13, ["5_7_13_15", "12_13_14_15"]),
              (14, ["2_6_10_14", "6_7_14_15", "8_10_12_14", "12_13_14_15"]),
              (15, ["5_7_13_15", "6_7_14_15", "12_13_14_15"]),
            ]
            |> List.map(((i, ts)) => (get(i), ts |> List.map(getI)))
            |> PIT.fromList,
            (
              ["0_2_8_10", "5_7_13_15", "6_7_14_15", "12_13_14_15"]
              |> List.map(getI),
              PIT.empty,
            ),
          ),
        ],
        ((input, (expectedEpis, expectedTable))) => {
          let (actualEpis, actualTable) = PIT.reduce(input);
          expect(
            List.eqBy(Term.eq, actualEpis, expectedEpis)
            && PIT.(actualTable |=| expectedTable),
          )
          |> toEqual(true);
        },
      );
    }
  )();

  testAll(
    "solve",
    [
      ([1, 2, 3, 4, 5, 6], ["-10", "0-1", "10-"]),
      (
        [0, 2, 8, 5, 6, 10, 12, 7, 13, 14, 15],
        ["-0-0", "-1-1", "-11-", "11--"],
      ),
      ([6, 5, 2, 3], ["101", "-10", "01-"]),
    ]
    |> List.map(((a, b)) =>
         (a, b |> List.map(Term.fromS) |> Term.S.fromList)
       ),
    ((input, expected)) => {
      let actual = solve(input);
      expect(Term.S.(actual |=| expected)) |> toEqual(true);
    },
  );

  testAll(
    "minimizeExpression",
    [
      // (A + B) * (A + C) => A + B + C
      (
        [
          (1, ["-1", "-01"]),
          (2, ["1-", "-10"]),
          (3, ["-1", "1-"]),
          (4, ["10-", "1-0"]),
          (5, ["-01", "10-"]),
          (6, ["-10", "1-0"]),
        ]
        |> List.map(((i, ss)) =>
             (Term.ofN(i), ss |> List.map(Term.fromS))
           )
        |> PIT.fromList,
        Bool.Or.from(
          [|
            [|"-10", "-1", "1-0", "-01"|],
            [|"1-", "-01", "1-0"|],
            [|"-10", "-1", "10-"|],
            [|"1-", "-1", "10-", "1-0"|],
            [|"1-", "-01", "10-", "-10"|],
          |]
          |> Array.map(Array.map(Bool.literal) >> Bool.And.from),
        ),
      ),
    ],
    ((input, expected)) => {
      let actual = PIT.minimizeExpression(input);
      expect(actual |> Option.map(Bool.eq(expected)))
      |> toEqual(Some(true));
    },
  );

  testAll(
    "extractShortestTerms",
    [
      (
        [|
          [|"-10", "-1", "1-0", "-01"|],
          [|"1-", "-01", "1-0"|],
          [|"-10", "-1", "10-"|],
          [|"1-", "-1", "10-", "1-0"|],
          [|"1-", "-01", "10-", "-10"|],
        |]
        |> Array.map(Array.map(Bool.literal) >> Bool.And.from)
        |> Bool.Or.from,
        ["-10", "-1", "10-"] |> List.map(Term.fromS) |> Term.S.fromList,
      ),
    ],
    ((input, expected)) => {
      let actual = PIT.extractShortestTerms(input);
      expect(Term.S.(actual |=| expected)) |> toEqual(true);
    },
  );
});
