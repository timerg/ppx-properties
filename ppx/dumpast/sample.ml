type my_t = {
  a: int;
  b: string;
} [@@deriving properties]


(* ocamlfind ppx_tools/rewriter _esy/default/build/default/ppx/ppx.exe dumpast/sample.ml *)