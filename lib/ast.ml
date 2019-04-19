type literal =
  [ `Numeric of string
  | `String of string
  ]
[@@deriving show, eq]

type computation = {
  identifier : string;
  parameters : literal list
}
[@@deriving show, eq]

type statement =
  | Anonymous of expression
  | Label of string * expression
  | Assignment of string * expression
[@@deriving show, eq]

and expression =
  | Computation of computation
  | Block of statement list
  | Override of expression * expression
  | Repeat of expression * int
  | Import of string (* An import statement *)
[@@deriving show, eq]
