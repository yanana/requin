open Relude;
open Jest;
open Expect;

open Function;

describe("expression", () => {
  test("foo", () => {
    expect(true) |> toBe(true)
  });
  let t = () => {
    open Requin_Expr;
    [@warning "-8"]
    let [@warning "-8"] [a, b, c, d, e, ..._] =
      Int.rangeAsList(1, 6)
      |> List.map(x => (x, x))
      |> List.map(compose(literal, uncurry2(Requin_Term.fromN)));

    testAll(
      "|*|",
      [
        (And.from([a, b, c]), c, And.from([a, b, c])),
        (And.from([a, b, c, d]), e, And.from([a, b, c, d, e])),
        (
          And.from([a, b]),
          Or.from([c, d]),
          Or.from([And.from([a, b, c]), And.from([a, b, d])]),
        ),
        (
          a,
          Or.from([b, c]),
          Or.from([And.from([a, b]), And.from([a, c])]),
        ),
        (
          Disjunctive.from([a, b]),
          Disjunctive.from([c, d]),
          Disjunctive.from([
            And.from([a, c]),
            And.from([a, d]),
            And.from([b, c]),
            And.from([b, d]),
          ]),
        ),
      ],
      ((a, b, expected)) =>
      expect(eq(a |*| b, expected)) |> toEqual(true)
    );

    testAll(
      "toDnf",
      [
        // AB => AB
        (a |*| b, a |*| b),
        // A * (B * C) => A * B * C
        (a |*| (b |*| c), a |*| b |*| c),
        // A * ((B * C) * D) => A * B * C * D
        (a |*| (b |*| c |*| d), a |*| b |*| c |*| d),
        // A * (B + C + D) => A * B + A * C + A * C + A * D
        (a |*| (b |+| c |+| d), a |*| b |+| (a |*| c) |+| (a |*| d)),
        // A + ((B + C) + D) => A + B + C + D
        ((a |+| (b |+| c |+| d)), Or.from([a, b, c, d])),
        // (A + B) * C => AC + BC
        (((a |+| b) |*| c), ((c |*| a) |+| (c |*| b))),
        // (A + B) * C * (D + E) => CAD + CAE + BCD + BCE
        (
          And.from([a |+| b, c, d |+| e]),
          Or.from([
            And.from([c, d, a]),
            And.from([c, d, b]),
            And.from([c, e, a]),
            And.from([c, e, b]),
          ]),
        ),
      ],
      ((input, expected)) => {
        let actual = toDnf(input);

        expect(actual |> eq(expected)) |> toEqual(true);
      },
    );
  };
  t();
});
