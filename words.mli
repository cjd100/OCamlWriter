(** [word_count s] takes in a string s and produces an int that
    signifies the number of distinct words (defined as a collection of
    alphanumeric characters delimited by whitespace) given in the string
    provided *)
val word_count : string -> int

(** [char_count s] takes in a string [s] and produces an int that
    specifies the number of non-whitespace alphanumeric characters in
    the string *)
val char_count : string -> int

(** [uniq_count s] takes in a string [s] and produces an int that
    specifies the number of unique words (collections of alphanumeric
    characters delimited by whitespace) in the string *)
val uniq_count : string -> int
