open Chibi_util
open OSeq

type t = (int * Bigstring.t)
type subscription_type = Schema.subscription_type 
type schema = Schema.subscription

let gets_content (f:Bigstring.t -> 'a) : (t -> 'a) = 
    (fun (_id, content) -> f content)

let get_created_on = gets_content Schema.subscription_get_created_on
let get_created_by = gets_content Schema.subscription_get_created_by
let get_next_check_timestamp = gets_content Schema.subscription_get_next_check_timestamp 
let get_uri = gets_content Schema.subscription_get_uri 
let get_most_recent_post_id = gets_content Schema.subscription_get_most_recent_post_id 
let get_subscription_type = gets_content Schema.subscription_get_subscription_type
let get_prev = gets_content Schema.subscription_get_prev
let get_deleted_on = gets_content Schema.subscription_get_deleted_on

let head_offset_key = "subscriptions_head"
let get_head () = App_state.get_offset head_offset_key 
let set_head v = App_state.set_offset head_offset_key v

let create (inp:schema) = 
    Log.with_write_lock (fun () -> 
        let now = Unix.time () |> Int64.of_float in 
        let prev_head = match get_head () with 
        | None -> Int64.zero 
        | Some v -> v in
        let inp = {inp with prev = prev_head;} in
        let buf = Schema.create_subscription_buffer inp now in 
        let new_id = Post_log.append_log_entry
            App_state.state.post_log
            Post_log.SUBSCRIPTION 
            buf in (
                set_head (Int64.of_int new_id);
                (new_id, buf)
            )
    )
    
let delete (id, _) = 
    let now = Unix.time () |> Int64.of_float in 
    Log.with_write_lock (fun () -> 
        Post_log.update_locked_log_entry_inplace (fun _st buf -> 
            Schema.subscription_set_deleted_on buf now
        ) App_state.state.post_log id
    )

let find_by_id id = 
    match Post_log.find_by_id id with 
    | (Post_log.SUBSCRIPTION, blob) -> Some (id, blob) 
    | _ -> None

let subs_seq subs = 
    filter_map (fun (id, typ, buf) -> 
        match typ with 
        | Post_log.SUBSCRIPTION -> Some (id, buf)
        | _ -> None
    ) subs

let all_from head = 
    Post_log.links_seq (fun id typ buf -> 
        match typ with 
        | Post_log.SUBSCRIPTION -> (
            match (get_prev (id, buf)) with
            | None -> None
            | Some v -> Some (Int64.to_int v)
        )
        | _ -> None
    ) head
    |> subs_seq

let all () = match get_head () with 
| None -> empty 
| Some h -> all_from (Int64.to_int h)

(* skip deleted subscriptions *)
let all_valid () =
    let now = Unix.time () |> Int64.of_float in 
    all () 
    |> filter (fun sub -> match (get_deleted_on sub) with 
        | None -> true
        | Some t -> t <= now
    )

let all_pending () = 
    let now = Unix.time () |> Int64.of_float in 
    all () 
    |> filter (fun sub -> 
        (get_next_check_timestamp sub) <= now
    )

