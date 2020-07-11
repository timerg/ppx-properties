

type t = {
  a: int;
  b: string;
}

type _ field =
  | A: int field
  | B: string field


let get:  type v. v field -> t -> v =
  function
   | A -> fun t -> t.a
   | B -> fun t -> t.b


let set: type v. v field -> v -> t -> t =
  function
  | A -> fun value t -> { t with a = value}
  | B -> fun value t -> { t with b = value}


let update field f t = set field ( f (get field t)) t



(* ocamlfind ppx_tools/dumpast dumpast/result.ml *)
