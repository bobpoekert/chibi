open Chibi_util 
open OSeq

type t = (int * Bigstring.t)
type schema = Schema.post

let default_t = (0, Bigstring.create 0)

let gets_post_content (thunk:Bigstring.t -> 'a) : (t -> 'a)= 
    (fun (_id, content) -> thunk content)
let gets_post_id (thunk:int -> 'a) : (t -> 'a)= 
    (fun (id, _content) -> thunk id)

let get_author = gets_post_content Schema.post_get_author
let get_content = gets_post_content Schema.post_get_content
let get_attachments = gets_post_content Schema.post_get_attachments
let get_topics = gets_post_content Schema.post_get_topics 
let get_created_on = gets_post_content Schema.post_get_created_on

let create (post:schema) : t =
    (*
    
        parent references we need to update when we add a log entry:
        - if this is an update, next_revision of prev entry (updates not implemented yet)
        - if this is a reply, next_reply of in_reply_to
        
        lmdb entries we need to update:
        - last_post_id of author
        - offset for each topic

    *)
    Log.with_write_lock (fun () -> 
        let res = ref default_t in 
        let _ = User.update_by_name post.author (fun author_state -> 
            let old_head_id = Schema.user_get_last_post_id author_state in

            let post = {post with prev_post_by_author = old_head_id;} in

            let buf = Schema.create_post_buffer post in 
            let new_id = Post_log.append_log_entry
                App_state.state.post_log
                Post_log.POST
                buf in

            (match post.in_reply_to with 
            | None -> ()
            | Some parent_id -> let _ = Post_log.update_locked_log_entry_inplace (fun _header body -> 
                Schema.post_set_next_reply body parent_id;
            ) App_state.state.post_log (Int64.to_int parent_id) in ());
            Schema.user_set_last_post_id author_state (Int64.of_int new_id);
            res := (new_id, author_state);
            author_state
        ) in !res
    ) App_state.state.post_log

let rec find_by_id id : t option = 
    if id == 0 then None else
    let l = App_state.state.post_log in 
    let typ, blob = Post_log.read_log_entry ~offset:id l in
    match typ with 
    | None -> None
    | Some typ ->  
    match typ with 
    | Post_log.LIKE -> None
    | Post_log.USER -> None
    | Post_log.POST -> 
        let next = Schema.post_get_next_revision blob |> Int64.to_int in 
        if next != 0 then find_by_id next
        else Some (id, blob) 

let rec links_seq (getter:t -> int option) id = 
    (fun () -> 
        match find_by_id id with
        | None -> Seq.Nil
        | Some post -> 
            match getter post with 
            | None -> Seq.Nil 
            | Some next_id -> 
                Seq.Cons (post, links_seq getter next_id)
    )

let returns_some f = (fun v -> Some (f v))

let retruns_i64_int f = (fun x -> Int64.to_int (f x))

let getter_seq g = 
    g
    |> retruns_i64_int 
    |> gets_post_content 
    |> returns_some
    |> links_seq

let all_with_author_id = getter_seq Schema.post_get_prev_post_by_author
let all_with_author user = 
    (fun () -> 
        match Schema.user_get_last_post_id user with
        | None -> Seq.Nil
        | Some id -> (all_with_author_id (Int64.to_int id)) ()
    )


let all_revisions = getter_seq Schema.post_get_prev_revision
let all_with_topic start_id topic_id = 
    let rec seq_fn post_id = 
        (fun () -> 
            match find_by_id post_id with 
            | None -> Seq.Nil 
            | Some post -> 
                let _id, pbuf = post in 
                match Schema.post_get_prev_post_by_topic pbuf topic_id with 
                | None -> Seq.Nil 
                | Some next_id -> Seq.Cons (post, seq_fn (Int64.to_int next_id))
        )in
    seq_fn start_id

(* gets the reply thread rooted at the given post *)
let reply_thread_downward = links_seq (fun (_post_id, pbuf) -> 
    match Schema.post_get_next_reply pbuf with 
    | None -> None 
    | Some v -> Some (Int64.to_int v)
)

let all_before = links_seq (fun (id, _content) -> 
    Post_log.prev_entry_id App_state.state.post_log id
)

(* the id of an entry is an offset to the end of the entry, 
   so the end of the log is the id of the last entry in the log
   *)
let all () = all_before (Log.end_off App_state.state.post_log)

let before_timestamp timestamp s =
    s
    |> drop_while (fun v -> (get_created_on v) >= timestamp)

let after_timestamp timestamp s = 
    s 
    |> take_while (fun v -> (get_created_on v) >= timestamp)

let after_id id s = 
    s 
    |> take_while (fun (target_id, _buf) -> target_id >= id)

let before_id id s = 
    s
    |> drop_while (fun (target_id, _buf) -> target_id > id)
