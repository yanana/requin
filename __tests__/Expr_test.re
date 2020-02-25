open Relude;
open Jest;
open Expect;

open Function;

describe("expression", () => {
  test("foo", () => {
    expect(true) |> toBe(true)
  });
  //   const input = List.of([0b001, 0b011], [0b100, 0b110], [0b011, 0b111], [0b110, 0b111]);
  // const expected = List.of([0b001, 0b010], [0b100, 0b010], [0b011, 0b100], [0b110, 0b001]);
  let t = () => {
    open Requin_Expr;
    [@warning "-8"]
    let [@warning "-8"] [a, b, c, d, e, ..._] =
      Int.rangeAsList(1, 6) |> List.map(x => (x, x)) |> List.map(compose(literal, uncurry2(Requin_Term.fromN)));

    // [and(a, b, c, d), e, and(a, b, c, d, e)],
    // [and(a, b), or(c, d), or(and(a, b, c), and(a, b, d))],
    // [a, or(b, c), or(and(a, b), and(a, c))],
    // [or(a, b), or(c, d), or(and(a, c), and(a, d), and(b, c), and(b, d))]
    testAll(
      "***",
      [
        (And.from([a, b, c]), c, And.from([a, b, c])),
        (And.from([a, b, c, d]), e, And.from([a, b, c, d, e])),
        (And.from([a, b]), Or.from([c, d]), Or.from([And.from([a, b, c]), And.from([a, b, d])])),
        (a, Or.from([b, c]), Or.from([And.from([a, b]), And.from([a, c])])),
        (
          Or.from([a, b]),
          Or.from([c, d]),
          Or.from([And.from([a, c]), And.from([a, d]), And.from([b, c]), And.from([b, d])]),
        ),
      ],
      ((a, b, expected)) =>
      expect(eq(a *** b, expected)) |> toEqual(true)
    );

    testAll(
      "toDnf",
      [
        // AB => AB
        (And.of2(a, b), And.of2(a, b)),
        // A * (B * C) => A * B * C
        (And.of2(a, And.of2(b, c)), And.from([a, b, c])),
        // A * ((B * C) * D) => A * B * C * D
        (And.from([a, And.of2(And.of2(b, c), d)]), And.from([a, b, c, d])),
        // A * (B + C + D) => A * B + A * C + A * C + A * D
        (And.of2(a, Or.from([b, c, d])), Or.from([And.of2(a, b), And.of2(a, c), And.of2(a, d)])),
        // A + ((B + C) + D) => A + B + C + D
        (Or.of2(a, Or.of2(Or.of2(b, c), d)), Or.from([a, b, c, d])),
        // (A + B) * C => AC + BC
        (And.of2(Or.of2(a, b), c), Or.of2(And.of2(c, a), And.of2(c, b))),
        // (A + B) * C * (D + E) => CAD + CAE + BCD + BCE
        (
          And.from([Or.of2(a, b), c, Or.of2(d, e)]),
          Or.from([And.from([c, d, a]), And.from([c, d, b]), And.from([c, e, a]), And.from([c, e, b])]),
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
