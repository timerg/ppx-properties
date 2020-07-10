open Base;
open Ppxlib;

let longident_loc_str = (~loc, s) => {
  {loc, txt: Lident(s)};
};
let loc_str = (~loc, s) => {
  {loc, txt: s};
};

module Field = {
  let makeField = (field: label_declaration): constructor_declaration => {
    let loc = field.pld_loc;
    let (module Builder) = Ast_builder.make(loc);
    open! Builder;

    let name = {
      ...field.pld_name,
      txt: field.pld_name.txt |> String.capitalize,
    };
    let args = Pcstr_tuple([]);

    let res = [%type: field([%t field.pld_type])];

    constructor_declaration(~name, ~args, ~res=Some(res));
  };

  let make = (~loc, _rec, fields) => {
    let (module Builder) = Ast_builder.make(loc);
    open! Builder;
    [
      type_declaration(
        ~name={txt: "field", loc},
        ~params=[(ptyp_any, Invariant)],
        ~cstrs=[],
        ~kind=Ptype_variant(List.map(~f=makeField, fields)),
        ~private_=Public,
        ~manifest=None,
      ),
    ]
    |> pstr_type(_rec);
  };
};

module type S = {
  let name: string;
  let make_constraint_type: (~loc: location, string) => core_type;
  let make_expr:
    (~loc: location, string, List.t(label_declaration)) => expression;
};

module MakeFunc = (S: S) => {
  let make =
      // funcName,
      // make_constraint_type,
      // make_expr,
      (~loc, typeName, fields: List.t(label_declaration)) => {
    let (module Builder) = Ast_builder.make(loc);
    open! Builder;

    let efunc =
      value_binding(
        ppat_constraint(
          ppat_var(loc_str(~loc, S.name)),
          S.make_constraint_type(~loc, typeName),
        ),
        S.make_expr(~loc, typeName, fields),
      );

    pstr_value(Nonrecursive, [efunc]);
  };
};

module Get =
  MakeFunc({
    let name = "get";

    let make_pattern = (~loc, typeName): pattern => {
      [%pat? (field, t)];
    };

    let make_expr_case = (field: label_declaration) => {
      let loc = field.pld_loc;
      let (module Builder) = Ast_builder.make(loc);
      open! Builder;
      let fieldName = field.pld_name.txt;
      let field = field.pld_name.txt |> String.capitalize;
      let lhs = ppat_construct({loc, txt: Lident(field)}, None);
      let rhs =
        pexp_fun(
          Nolabel,
          None,
          ppat_var(loc_str(~loc, "t")),
          pexp_field([%expr t], {txt: Lident(fieldName), loc}),
        );

      case(~lhs, ~guard=None, ~rhs);
    };

    let make_constraint_type = (~loc, typeName) => {
      // make type annotation of function
      let (module Builder) = Ast_builder.make(loc);
      open! Builder;

      let core_type =
        ptyp_arrow(
          Nolabel,
          ptyp_constr(longident_loc_str(~loc, "field"), [ptyp_var("v")]),
          ptyp_arrow(
            Nolabel,
            ptyp_constr(longident_loc_str(~loc, typeName), []),
            ptyp_var("v"),
          ),
        );

      ptyp_poly([{txt: "v", loc}], core_type);
    };

    let make_expr = (~loc, typeName, fields) => {
      let (module Builder) = Ast_builder.make(loc);
      open! Builder;

      let ptype_type_name =
        ptyp_constr(longident_loc_str(~loc, typeName), []);

      pexp_newtype(
        loc_str(~loc, "v"),
        pexp_constraint(
          pexp_function(List.map(~f=make_expr_case, fields)),
          ptyp_arrow(
            Nolabel,
            [%type: field(v)],
            ptyp_arrow(Nolabel, ptype_type_name, [%type: v]),
          ),
        ),
      );
    };
  });

module Set =
  MakeFunc({
    let name = "set";
    let make_constraint_type = (~loc, typeName) => {
      let (module Builder) = Ast_builder.make(loc);
      open! Builder;

      let core_type =
        ptyp_arrow(
          Nolabel,
          ptyp_constr(longident_loc_str(~loc, "field"), [ptyp_var("v")]),
          ptyp_arrow(
            Nolabel,
            ptyp_var("v"),
            ptyp_arrow(
              Nolabel,
              ptyp_constr(longident_loc_str(~loc, typeName), []),
              ptyp_constr(longident_loc_str(~loc, typeName), []),
            ),
          ),
        );

      ptyp_poly([{txt: "v", loc}], core_type);
    };

    let make_expr_case = (field: label_declaration) => {
      let loc = field.pld_loc;
      let (module Builder) = Ast_builder.make(loc);
      open! Builder;
      let fieldName = field.pld_name.txt;
      let field = field.pld_name.txt |> String.capitalize;
      let lhs = ppat_construct({loc, txt: Lident(field)}, None);
      let rhs = [%expr
        (value, t) => [%e
          pexp_record(
            [(longident_loc_str(~loc, fieldName), [%expr value])],
            Some([%expr t]),
          )
        ]
      ];
      case(~lhs, ~guard=None, ~rhs);
    };

    let make_expr = (~loc, typeName, fields) => {
      let (module Builder) = Ast_builder.make(loc);
      open! Builder;

      let ptype_type_name =
        ptyp_constr(longident_loc_str(~loc, typeName), []);

      pexp_newtype(
        loc_str(~loc, "v"),
        pexp_constraint(
          pexp_function(List.map(~f=make_expr_case, fields)),
          ptyp_arrow(
            Nolabel,
            [%type: field(v)],
            ptyp_arrow(
              Nolabel,
              [%type: v],
              ptyp_arrow(Nolabel, ptype_type_name, ptype_type_name),
            ),
          ),
        ),
      );
    };
  });

module Update = {
  let make = (~loc) => {
    let (module Builder) = Ast_builder.make(loc);
    open! Builder;

    let efunc =
      value_binding(
        ~pat=ppat_var(loc_str(~loc, "update")),
        ~expr=[%expr (field, f, t) => set(field, f(get(field, t)), t)],
      );

    pstr_value(Nonrecursive, [efunc]);
  };
};

let gen =
    (
      loc: Location.t,
      _rec: rec_flag,
      t: type_declaration,
      fields: List.t(label_declaration),
    )
    : list(structure_item) => {
  let typeName = t.ptype_name.txt;

  let type_fields = Field.make(~loc, _rec, fields);
  let func_get = Get.make(~loc, typeName, fields);
  let func_set = Set.make(~loc, typeName, fields);
  let func_update = Update.make(~loc);

  [type_fields, func_get, func_set, func_update];
};