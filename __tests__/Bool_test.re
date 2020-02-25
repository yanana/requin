open Jest;
open Expect;

module List = Relude.List;
module Array = Relude.Array;
module Set = Relude.Set;
module Option = Relude.Option;
module Map = Relude.Map;

// open Relude.Function;
// open Relude.Function.Infix;

module Bool = Requin_Bool;
open Bool;

describe("expr", () => {
  test("eq results in false", () => {
    let e =
      And.from([|
        literal("a"),
        literal("b"),
        Or.from([|And.from([|literal("a"), literal("c")|]), And.from([|literal("c"), literal("d")|])|]),
      |]);
    let e' =
      And.from([|
        literal("b"),
        literal("a"),
        Or.from([|And.from([|literal("c"), literal("d")|]), And.from([|literal("a"), literal("b")|])|]),
      |]);
    expect(e |=| e') |> toEqual(false);
  });

  test("==== results in true", () => {
    let e =
      And.from([|
        literal("a"),
        literal("b"),
        Or.from([|And.from([|literal("a"), literal("b")|]), And.from([|literal("c"), literal("d")|])|]),
      |]);
    let e' =
      And.from([|
        literal("b"),
        literal("a"),
        Or.from([|And.from([|literal("c"), literal("d")|]), And.from([|literal("a"), literal("b")|])|]),
      |]);
    expect(e |=| e') |> toEqual(true);
  });

  test("literal *** literal", () =>
    expect(literal("a") *** literal("b")) |> toEqual(And.from([|literal("a"), literal("b")|]))
  );

  test("literal *** And", () => {
    let actual = literal("a") *** And.from([|literal("b"), literal("c")|]);
    let expected = And.from([|literal("a"), literal("b"), literal("c")|]);
    expect(actual |=| expected) |> toEqual(true);
  });

  [@warning "-8"]
  let [p1, p2, p3, p4, p5, p6, ..._] =
    Belt.Array.range(1, 6)->Belt.List.fromArray->(Belt.List.map(i => literal({j|p$i|j})));
  [@warning "+8"]
  let tt = [
    (Or.from([|p1, p3, p4|]), Or.from([|literal("p1"), literal("p3"), literal("p4")|])),
    (Or.from([|p1, p2|]) *** p3, Or.from([|And.from([|p1, p3|]), And.from([|p2, p3|])|])),
    (Or.from([|p1, p2|]) *** Or.from([|p1, p3|]), Or.from([|p1, And.from([|p2, p3|])|])),
    (
      Or.from([|And.from([|p1, p2, p5, p4|]), And.from([|p6, p5|])|]) *** Or.from([|p2, p4|]),
      Or.from([|And.from([|p1, p2, p5, p4|]), And.from([|p2, p6, p5|]), And.from([|p6, p5, p4|])|]),
    ),
    (
      Or.from([|p1, p2|]) *** Or.from([|p3, p4|]),
      Or.from([|And.from([|p1, p3|]), And.from([|p1, p4|]), And.from([|p2, p3|]), And.from([|p2, p4|])|]),
    ),
    (
      Or.from([|p1, p2|]) *** (Or.from([|p3, p4|]) *** Or.from([|p1, p3|])),
      Or.from([|And.from([|p1, p3|]), And.from([|p1, p4|]), And.from([|p2, p3|])|]),
    ),
    (
      Or.from([|p1, p2|])
      *** (
        Or.from([|p3, p4|])
        *** (Or.from([|p1, p3|]) *** (Or.from([|p5, p6|]) *** (Or.from([|p2, p5|]) *** Or.from([|p4, p6|]))))
      ),
      Or.from([|
        And.from([|p1, p4, p5|]),
        And.from([|p2, p3, p6|]),
        And.from([|p1, p2, p4, p6|]),
        And.from([|p1, p3, p5, p6|]),
        And.from([|p2, p3, p4, p5|]),
      |]),
    ),
    (
      Or.from([|p2, p1|])
      *** (
        Or.from([|p4, p3|])
        *** (Or.from([|p1, p3|]) *** (Or.from([|p5, p6|]) *** (Or.from([|p2, p5|]) *** Or.from([|p6, p4|]))))
      ),
      Or.from([|
        And.from([|p1, p4, p5|]),
        And.from([|p2, p3, p6|]),
        And.from([|p1, p2, p4, p6|]),
        And.from([|p1, p3, p5, p6|]),
        And.from([|p2, p3, p4, p5|]),
      |]),
    ),
  ];

  testAll("***", tt, ((actual, expected)) => {expect(actual |=| expected) |> toEqual(true)});
});

describe("minimize", () => {
  test("simple", () => {
    let input =
      Bool.S.empty->(Belt.Set.add(literal("a")))->(Belt.Set.add(And.from([|literal("b"), literal("a")|])));
    let expected = Bool.S.empty |> Set.add(literal("a"));
    expect(Set.eq(minimize(input), expected)) |> toBe(true);
  });
  test("complex", () => {
    let input =
      Belt.Set.fromArray(
        ~id=Bool.S.id,
        [|
          And.from([|literal("a"), literal("b")|]),
          And.from([|literal("b"), literal("c"), literal("a")|]),
          And.from([|literal("d"), literal("c"), literal("b"), literal("a")|]),
        |],
      );
    let expected = Belt.Set.fromArray(~id=Bool.S.id, [|And.from([|literal("a"), literal("b")|])|]);
    expect(Set.eq(minimize(input), expected)) |> toBe(true);
  });
});

describe("Or", () => {
  [@warning "-8"]
  let [p1, p3, p4]: list(t) = ["p1", "p3", "p4"] |> List.map(literal);

  testAll(
    "from",
    [
      (
        [|p1, `And([p1, p4] |> S.fromList), `And([p1, p3] |> S.fromList), `And([p3, p4] |> S.fromList)|],
        `Or([p1, `And([p3, p4] |> S.fromList)] |> S.fromList),
      ),
    ],
    ((input, expected)) => {
      let actual = Or.from(input);
      expect(actual |=| expected) |> toEqual(true);
    },
  );
});
