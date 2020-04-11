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
        let created_on = Unix.time () |> Int64.of_float in 
        let old_head_id = App_state.get_user_offset post.author in

        let post = {post with prev_post_by_author = old_head_id;} in


        let old_topic_heads = Array.map (fun topic -> 
            match App_state.get_topic_offset topic with 
            | None -> Int64.zero 
            | Some v -> v
        ) post.topics in

        let post = {post with prev_posts_by_topic = old_topic_heads;} in

        let prev = match post.in_reply_to with 
        | None -> None
        | Some parent_id ->
            let _, parent = Post_log.find_by_id (Int64.to_int parent_id) in 
            match Schema.post_get_reply_child_list_head parent with
            | Some old_head -> Some old_head
            | None -> None
        in
        let post = {post with reply_child_list_prev = prev;} in
        let buf = Schema.create_post_buffer post created_on in 
        let new_id = Post_log.append_log_entry
            App_state.state.post_log
            Post_log.POST
            buf in

        (match post.in_reply_to with 
        | None -> ()
        | Some parent_id -> 
            Post_log.update_locked_log_entry_inplace (fun _st buf -> 
                Schema.post_set_reply_child_list_head buf (Int64.of_int new_id)
            ) App_state.state.post_log (Int64.to_int parent_id));

        Array.iter (fun topic -> 
            App_state.set_topic_offset topic (Int64.of_int new_id);
        ) post.topics;

        App_state.set_user_offset post.author (Int64.of_int new_id);

        (new_id, buf)
    ) App_state.state.post_log

let rec find_by_id id = 
    match Post_log.find_by_id id with 
    | (Post_log.POST, blob) -> 
        let next = Schema.post_get_next_revision blob |> Int64.to_int in 
        if next != 0 then find_by_id next
        else Some (id, blob)
    | _ -> None


let returns_some f = (fun v -> Some (f v))

let retruns_i64_int f = (fun x -> Int64.to_int (f x))

let posts_seq entries = 
    filter_map (fun (id, typ, buf) -> 
        match typ with 
        | Post_log.POST -> Some (id, buf)
        | _ -> None
    ) entries

let getter_seq (g:Bigstring.t -> int64) start_id : t Seq.t = 
    Post_log.links_seq (fun _id typ buf -> 
        match typ with 
        | Post_log.POST -> Some (Int64.to_int (g buf))
        | _ -> None
    ) start_id
    |> posts_seq

let all_with_author_id = getter_seq Schema.post_get_prev_post_by_author
let all_with_author user = 
    try (
        match App_state.get_user_offset user with
        | None -> empty 
        | Some user -> all_with_author_id (Int64.to_int user)
    ) with Not_found -> empty


let all_revisions = getter_seq Schema.post_get_prev_revision
let all_with_topic topic = 
    let rec seq_fn post_id = 
        (fun () -> 
            match Post_log.find_by_id post_id with 
            | (Post_log.POST, pbuf) -> (
                match Schema.post_get_prev_post_by_topic pbuf topic with 
                | None -> Seq.Cons ((post_id, pbuf), empty)
                | Some next_id -> Seq.Cons ((post_id, pbuf), seq_fn (Int64.to_int next_id))
            )
            | _ -> Seq.Nil
        )in
    match App_state.get_topic_offset topic with 
    | None -> empty 
    | Some start_id -> seq_fn (Int64.to_int start_id)

let get_immediate_replies root = 
    let _root_id, root_buf = root in 
    match Schema.post_get_reply_child_list_head root_buf with 
    | None -> (fun () -> Seq.Nil)
    | Some head -> Post_log.links_seq (fun _id entry_type entry -> 
        match entry_type with 
        | Post_log.POST -> (
            match Schema.post_get_reply_child_list_prev entry with 
            | None -> None 
            | Some v -> Some (Int64.to_int v)
        )
        | _ -> None
    ) (Int64.to_int head)
    |> posts_seq

type reply_tree = Reply_node of (t * reply_tree Seq.t)

let rec get_reply_tree v : reply_tree = 
    Reply_node (
        v, 
        map get_reply_tree (get_immediate_replies v)
    )

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
