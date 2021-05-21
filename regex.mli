(* This module implements regular expression operations *)

(** [replace_reg reg n str] is the same string as [str] except that all
    instances of the regular expression represented by [reg] are
    replaced by string [n]. *)
val replace_reg : string -> string -> string -> string

(** [replace_reg_first reg n str] is the same string as [str] except
    that the first instance of the regular expression represented by
    [reg] is replaced by string [n]. *)
val replace_reg_first : string -> string -> string -> string

(** [find reg str ind] is the index of the first occurence of the
    regular expression represented by [reg] after index [ind] in the
    string [str]. If no occurence is found, [find reg str ind] is -1. *)
val find_reg : string -> string -> int -> int

(** [replace_exact r n str] is the same string as [str] except that all
    instances of the string [r] are replaced by string [n]. *)
val replace_exact : string -> string -> string -> string

(** [replace_exact_first r n str] is the same string as [str] except
    that the first instance of the string [r] is replaced by string [n]. *)
val replace_exact_first : string -> string -> string -> string

(** [find r str ind] is the index of the first occurence of the string
    [r] after index [ind] in the string [str]. If no occurence is found,
    [find r str ind] is -1.*)
val find_exact : string -> string -> int -> int
