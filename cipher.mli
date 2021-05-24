(** Cipher is a module that implements file encryption/decryption *)

(** [encrypt key plain_text] is the text of [plain_text] encrypted into
    some indistinguishable form using [key]. Transforms plain text into
    cipher text. *)
val encrypt : string -> string -> string

(** [decrypt key cipher_text] is the text of [cipher_text] decrypted
    into a readable form using [key]. Transforms cipher text into plain
    text. Raises: *)
val decrypt : string -> string -> string
