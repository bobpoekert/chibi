
type log 

val get_buffer : log -> int -> Bigstring.t
val get_fd : log -> int -> Unix.file_descr

val open_or_create_dir : string -> int -> log

val read : log -> int -> int -> Bigstring.t
val overwrite : log -> Bigstring.t -> int -> unit
val append : log -> Bigstring.t -> int
val read_end : log -> int -> Bigstring.t


val end_off : log -> int

val with_write_lock : (unit -> 'a) -> log -> 'a