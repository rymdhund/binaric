type 'a t

val apply: ('a -> 'b) t -> 'a t -> 'b t

val (<*>): ('a -> 'b) t -> 'a t -> 'b t

val pure: 'a -> 'a t

val program: ?name:string -> 'a -> 'a t

val run: unit t -> unit

val flag: ?doc:string -> ?alias:string list -> string -> bool t

val flag_param: ?doc:string -> ?alias:string list -> string -> string -> string option t

val positional: ?doc:string -> string -> string t
