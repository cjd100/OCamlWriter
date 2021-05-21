(** [word_count s] takes in a string s and produces an int that
    signifies the number of distinct words (defined as a collection of
    alphanumeric characters delimited by whitespace) given in the string
    provided *)
val word_count : string -> int

(** [char_count s] takes in a string [s] and produces an int that
    specifies the number of non-whitespace alphanumeric characters in
    the string *)
val char_count : string -> int

(** [remove_dups lst] takes in an 'a list [lst] and removes all
    duplicate values in the list. Therefore, it creates a set-like list
    with completely unique values. Does not preserve order of elements *)
val remove_dups : 'a list -> 'a list

(** [uniq_count s] takes in a string [s] and produces an int that
    specifies the number of unique words (collections of alphanumeric
    characters delimited by whitespace) in the string *)
val uniq_count : string -> int
