open Base;
open Ppxlib;
module Args = Ppxlib.Deriving.Args;
module Import = Ppxlib__.Import;

let str_gen =
    (
      ~loc,
      ~path as _,
      (_rec: rec_flag, exprs: list(type_declaration)),
      pipeFirst: bool,
    ) => {
  let t = Base.List.hd_exn(exprs); // we dont handle recursive type

  switch (t.ptype_kind) {
  | Ptype_record(fields) =>
    pipeFirst ? Obj.magic("") : Lenses_Gen.gen(loc, _rec, t, fields)
  | _ => Location.raise_errorf(~loc, "deriveFunction only works on records.")
  };
};

let name = "properties";

let () = {
  let args: Args.t(Import.bool => Import.structure, Import.structure) =
    Args.(empty +> flag("pipeFirst"));

  let str_type_decl = Deriving.Generator.make(args, str_gen);

  Deriving.add(name, ~str_type_decl) |> Deriving.ignore;
};

/**/;