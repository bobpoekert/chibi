
type log 

val open_or_create_dir : string -> int -> log

val read : log -> int -> int -> Bigstring.t
val overwrite : log -> Bigstring.t -> int -> unit
val append : log -> Bigstring.t -> unit
val read_end : log -> int -> Bigstring.t

val with_write_lock : (unit -> 'a) -> log -> 'a