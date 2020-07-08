open Jest;
open ExpectJs;

[@deriving properties]
type my_typ = {
  a: int,
  b: string,
  c: int,
  nested: option(my_typ),
};

let my_typ = {
  a: 0,
  b: "hello",
  c: 1,
  nested: Some({a: 0, b: "hello", c: 1, nested: None}),
};

let _ =
  describe("test", () => {
    test("get", () => {
      (get(A, my_typ), get(B, my_typ), get(C, my_typ))
      |> expect
      |> toEqual((0, "hello", 1))
    });
    test("set", () => {
      my_typ
      |> set(A, 2)
      |> set(B, "world")
      |> set(C, 3)
      |> expect
      |> toEqual({a: 2, b: "world", c: 3, nested: my_typ.nested})
    });
    test("update", () => {
      my_typ
      |> update(A, i => i + 1)
      |> update(B, s => s ++ " world")
      |> update(C, i => i * 2)
      |> expect
      |> toEqual({a: 1, b: "hello world", c: 2, nested: my_typ.nested})
    });

    test("update nested", () => {
      let map = (f, o) => Belt.Option.map(o, f);

      my_typ
      |> update(Nested, map(update(A, i => i + 1)))
      |> expect
      |> toEqual({
           ...my_typ,
           nested: Some({a: 1, b: "hello", c: 1, nested: None}),
         });
    });
  });