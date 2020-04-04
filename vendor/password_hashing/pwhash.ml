
external generate_key : unit -> string = "call_generate_key"
external hash_password : string -> string -> string = "call_hash_password"
external verify_password : string -> string -> string -> bool = "call_verify_password"
