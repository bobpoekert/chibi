
type user_type = 
| LOCAL 
| RSS
| ACTIVITYPUB 
| EMAIL (* for newsletters *)

type attachment_type = 
| IMAGE 
| AUDIO
| VIDEO 
| BLOB
| OTHER

type post_content_type = 
| MARKDOWN
| HTML
| PLAIN_TEXT

(* users *)

type user = {
    password_hash : string option;
    bio : string option;
    signature : string option;
    avatar_id : int64 option;
    last_post_id : int64 option;
    remote_uri : string option;
    update_interval : int64 option;
    name : string;
    user_type : user_type;
}

val default_user : user

val user_get_name : Cstruct.buffer -> string option 
val user_get_bio : Cstruct.buffer -> string option 
val user_get_signature : Cstruct.buffer -> string option 
val user_get_remote_uri : Cstruct.buffer -> string option

val user_get_created_on : Cstruct.buffer -> int64
val user_get_last_update_time : Cstruct.buffer -> int64
val user_get_update_interval : Cstruct.buffer -> int64
val user_get_type : Cstruct.buffer -> user_type option
val user_get_last_post_id : Cstruct.buffer -> int64 option

val user_set_created_on : Cstruct.buffer -> int64 -> unit
val user_set_last_update_time : Cstruct.buffer -> int64 -> unit
val user_set_update_interval : Cstruct.buffer -> int64 -> unit
val user_set_last_post_id : Cstruct.buffer -> int64 -> unit

val create_user_buffer : user -> Cstruct.buffer

(* posts *)

type attachment_record = {
    kind : attachment_type;
    id : int;
    size : int;
}

type post = {
    created_on : int64;
    author : string;
    content : string option;
    attachments : attachment_record array;
    prev_post_by_author : int64 option;
    topics : int32 array;
    prev_posts_by_topic : int64 array;
    in_reply_to : int64 option;
    next_reply : int64 option;
    deleted_on : int64 option;
}

val post_get_author : Bigstring.t -> string option
val post_get_content : Bigstring.t -> string option 
val post_get_attachments : Bigstring.t -> attachment_record array
val post_get_topics : Bigstring.t -> int32 array 
val post_get_created_on : Bigstring.t -> int64
val post_get_prev_revision : Bigstring.t -> int64
val post_get_next_revision : Bigstring.t -> int64
val post_get_prev_post_by_author : Bigstring.t -> int64
val post_get_prev_post_by_topic : Bigstring.t -> int32 -> int64 option
val post_get_in_reply_to : Bigstring.t -> int64 option 
val post_get_next_reply : Bigstring.t -> int64 option

val post_set_created_on : Bigstring.t -> int64 -> unit 
val post_set_prev_post_by_author : Bigstring.t -> int64 -> unit 
val post_set_prev_revision : Bigstring.t -> int64 -> unit 
val post_set_next_revision : Bigstring.t -> int64 -> unit
val post_set_in_reply_to : Bigstring.t -> int64 -> unit 
val post_set_next_reply : Bigstring.t -> int64 -> unit

val create_post_buffer : post -> Bigstring.t

type 'a packer = Bigstring.t -> int -> 'a -> unit

val pack_int_array : int -> 'a packer -> 'a array -> Bigstring.t