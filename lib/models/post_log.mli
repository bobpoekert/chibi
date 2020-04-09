open Chibi_util

type log_entry_type = 
| POST 
| LIKE 
| USER

val read_log_entry : ?offset:int -> Log.log -> (log_entry_type option * Bigstring.t)
val append_log_entry : Log.log -> log_entry_type -> Bigstring.t -> int 
val update_log_entry : (Bigstring.t -> Bigstring.t) -> Log.log -> int -> unit
val prev_entry_id : Log.log -> int -> int option