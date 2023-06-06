type t 

(* Constructor from string *)
val init : string -> t

(* Next token in the stream *)
val next_token : t -> (t * Token.t option)
val token_list : t -> Token.t list -> Token.t list

(* Pretty printing *)
val pp : Format.formatter -> t -> unit
val show : t -> string

 