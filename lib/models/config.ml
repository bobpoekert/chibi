
let cache = Hashtbl.create 1000 

type t = App_state.config_type

let get k = 
    match Hashtbl.find_opt cache k with 
    | Some v -> v 
    | None -> let v = App_state.get_config_param k in (
        Hashtbl.add cache k v;
        v
    )

let set k v = 
    App_state.set_config_param k v

let get_string k = 
    match get k with 
    | String v -> v 
    | _ -> raise (Failure "expected string, got something else")
let get_int k = 
    match get k with 
    | Int v -> v 
    | _ -> raise (Failure "expected string, got something else")

let set_string k v = set k (String v)
let set_int k v = set k (Int v)