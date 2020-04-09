
type t = Cstruct.buffer
type schema = Schema.user

type user_type = Schema.user_type

let users_table = 
    let db = App_state.state.db in 
    Lmdb.Map.create Lmdb.Map.Nodup
        ~name:"users" ~key:Lmdb.Conv.string ~value:Lmdb.Conv.bigstring db

let default = Schema.default_user

let create (user:schema) : t =
    let buf = Schema.create_user_buffer user in
    Lmdb.Map.set users_table user.name buf; buf

let find_by_name name : t option = 
    try Some (Lmdb.Map.get users_table name)
    with Not_found -> None

let update_by_name name thunk = 
    Lmdb.Txn.go Lmdb.Rw App_state.state.db (fun txn -> 
        let prev = Lmdb.Map.get ~txn:txn users_table name in 
        let next = thunk prev in 
        Lmdb.Map.set ~txn:txn users_table name next
    )


let get_name = Schema.user_get_name
let get_bio = Schema.user_get_bio
let get_signature = Schema.user_get_signature
let get_remote_uri = Schema.user_get_remote_uri

let get_created_on = Schema.user_get_created_on 
let get_last_update_time = Schema.user_get_last_update_time 
let get_update_interval = Schema.user_get_update_interval 
let get_type = Schema.user_get_type
