open Chibi_util


type log_entry_type = 
| POST 
| LIKE 
| USER
| SUBSCRIPTION 
| UNKNOWN

type log_entry = (int * log_entry_type * Bigstring.t)

val read_log_entry : ?offset:int -> Log.log -> (log_entry_type * Bigstring.t)
val append_log_entry : Log.log -> log_entry_type -> Bigstring.t -> int 
val update_locked_log_entry_inplace : (Cstruct.t -> Bigstring.t -> unit) -> Log.log -> int -> unit
val prev_entry_id : Log.log -> int -> int option

val find_by_id : int -> (log_entry_type * Bigstring.t)
val links_seq : (int -> log_entry_type -> Bigstring.t -> int option) -> int -> log_entry Seq.t
val all_before : int -> log_entry Seq.t
val all : unit -> log_entry Seq.t