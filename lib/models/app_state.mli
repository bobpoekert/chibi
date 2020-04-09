open Chibi_util

type config_type = 
    | Int of int
    | String of string

type app_state = {
    db: Lmdb.Env.t;
    config_data: (string, config_type, [`Uni]) Lmdb.Map.t;
    offsets: (string, int64, [`Uni]) Lmdb.Map.t;
    post_log: Log.log;
    attachment_log: Log.log;
}

val state : app_state 

val read_attachment : int -> Bigstring.t
val add_attachment : Bigstring.t -> int

val get_offset : string -> int64 
val set_offset : string -> int64 -> unit

val get_config_param : string -> config_type 
val set_config_param : string -> config_type -> unit