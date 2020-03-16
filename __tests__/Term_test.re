open Jest;
open Expect;

module Term = Requin_Term;

open Term;

describe("Term", () => {
  testAll(
    "countSetBits",
    [(0, 0), (5, 2), (25, 3), (63, 6)],
    ((input, expected)) =>
    expect(countSetBits(input)) |> toEqual(expected)
  );

  testAll(
    "fromS",
    [("000", (0, 0)), ("--101-0", (20, 98))],
    ((input, (v, m))) => {
    expect(fromS(input)) |> toEqual(Term(v, m))
  });

  testAll(
    "fromN",
    [
      ((1, 3), (1, 2)),
      ((4, 6), (4, 2)),
      ((3, 7), (3, 4)),
      ((6, 7), (6, 1)),
    ],
    (((v, m), (v', m'))) => {
    expect(Term.fromN(v, m)) |> toEqual(Term(v', m'))
  });

  testAll(
    "toString",
    [("--101-0", "--101-0", 7), ("0010-", "0010-", 5)],
    ((input, expected, l)) => {
    expect(fromS(input) |> Term.toString(~length=l)) |> toEqual(expected)
  });

  testAll("toString2", [(Term(1, 0), "1")], ((t, expected)) =>
    expect(toString(t)) |> toEqual(expected)
  );

  testAll(
    "combine",
    [
      (fromS("001"), fromS("011"), fromS("0-1")),
      (fromS("1001"), fromS("1101"), fromS("1-01")),
      (fromS("110-"), fromS("111-"), fromS("11--")),
    ],
    ((a, b, expected)) => {
    expect(combine(a, b)) |> toEqual(Some(expected))
  });

  testAll(
    "isCombinable",
    [
      (fromS("001"), fromS("011"), true),
      (fromS("1001"), fromS("1101"), true),
      (fromS("1011"), fromS("0001"), false),
      (fromS("1-0"), fromS("11-"), false),
      (fromS("110-"), fromS("111-"), true),
    ],
    ((a, b, expected)) => {
    expect(isCombinable(a, b)) |> toEqual(expected)
  });

  testAll(
    "toBooleanExpression",
    [
      (fromS("--1-"), ["A", "B", "C", "D"], "C"),
      (
        fromS("100-01"),
        ["いろは", "にほへと", "ちり", "ぬ", "る", "を"],
        "いろは AND NOT にほへと AND NOT ちり AND NOT る AND を",
      ),
    ],
    ((input, variables, expected)) =>
    expect(toBooleanExpression(variables, input)) |> toEqual(expected)
  );
});
