open Bigarray
open EndianBigstring
open BatPervasives


[%%cenum
type user_type = 
| LOCAL 
| RSS
| ACTIVITYPUB 
| EMAIL (* for newsletters *)
[@@uint8_t]]

[%%cenum
type attachment_type = 
| IMAGE 
| AUDIO
| VIDEO 
| BLOB
| OTHER
[@@uint8_t]]

let decode_attachment_type v = 
    match int_to_attachment_type v with 
    | None -> OTHER 
    | Some v -> v

[%%cenum 
type post_content_type = 
| MARKDOWN
| HTML
| PLAIN_TEXT
[@@uint8_t]]

(* 
    variable length data is appended to the end of structs,
    the fields that reference that data are a pair of offset, length ints
 *)

let user_version = 1;
[%%cstruct 
type user = {
    (* version 1 *)
    version: uint16_t;
    id: uint64_t;
    name_offset: uint32_t;
    name_length: uint32_t;
    password_hash: uint8_t [@len 32];
    created_on: uint64_t;
    bio_offset: uint32_t;
    bio_length: uint32_t;
    signature_offset: uint32_t;
    signature_length: uint32_t;
    avatar_id: uint64_t;
    most_recent_post_id: uint64_t;
    most_recent_like_id: uint64_t;
    user_type: uint8_t;
    remote_uri_offset: uint32_t;
    remote_uri_length: uint32_t;
    last_update_time: uint64_t;
    update_interval: uint64_t;

    (* version 2 will go here *)
} [@@little_endian]]


let varchar_getter offset_getter length_getter = 
    fun blob -> 
        let st = Cstruct.of_bigarray blob in
        let offset = offset_getter st in 
        let length = length_getter st in
        if length > 0 then 
            Some (Array1.sub blob offset length)
        else None

let string_getter a b = 
    let g = varchar_getter a b in 
    fun blob -> 
        match g blob with 
        | Some v -> Some (Bigstring.to_string v)
        | None -> None

let array_getter offset_getter length_getter int_default int_width unpacker = 
    fun blob ->
        let st = Cstruct.of_bigarray blob in
        let length = length_getter st in 
        if length < 1 then [| |] else 
        let offset = offset_getter st in 
        let res = Array.make length int_default in (
            for idx = 0 to length do 
                Array.set res idx (unpacker blob (offset + (idx * int_width)))
            done; 
            res
        )

type buffer_builder = {
    st: Cstruct.t;
    buffers: Bigstring.t list;
    size: int;
}

let buffer_builder_create struct_size = {
    st = Cstruct.create struct_size;
    buffers = [];
    size = struct_size;
}

let buffer_builder_pack builder = 
    let st_buf = Cstruct.to_bigarray builder.st in 
    let res = Bigstring.create builder.size in 
    let _ = Bigstring.blit st_buf 0 res 0 builder.size in 
    let _ = List.fold_right (fun b off ->
        let size = Array1.dim b in (
            Bigstring.blit b 0 res off size;
            off + size
        )
    ) builder.buffers 0 in 
    res

let varchar_setter offset_setter length_setter = 
    fun builder buf ->
        let length = Array1.dim buf in 
        offset_setter builder.st builder.size;
        length_setter builder.st length;
        {
            st = builder.st;
            buffers = buf :: builder.buffers;
            size = builder.size + length;
        }

let string_setter offset_setter length_setter = 
    let vc = varchar_setter offset_setter length_setter in 
    fun builder s -> vc builder (Bigstring.of_string s)

type 'a packer = Bigstring.t -> int -> 'a -> unit
let pack_int_array int_width packer data = 
    let length = Array.length data in 
    let res = Bigstring.create (int_width * length) in 
    for idx = 0 to length do 
        packer res (idx * int_width) (Array.get data idx)
    done;
    res

let array_setter offset_setter length_setter int_width packer = 
    fun builder vs -> 
        let length = Array.length vs in
        let res = pack_int_array int_width packer vs in
        let new_off = builder.size + (length * int_width) in
        offset_setter builder.st new_off;
        length_setter builder.st length;
        {
            st = builder.st;
            buffers = res :: builder.buffers;
            size = new_off;
        }

let user_get_name = string_getter 
    (get_user_name_offset %> Int32.to_int)
    (get_user_name_length %> Int32.to_int)
let user_get_bio = string_getter
    (get_user_bio_offset %> Int32.to_int)
    (get_user_bio_length %> Int32.to_int)
let user_get_signature = string_getter 
    (get_user_signature_offset %> Int32.to_int)
    (get_user_signature_length %> Int32.to_int)
let user_get_remote_uri = string_getter
    (get_user_remote_uri_offset %> Int32.to_int)
    (get_user_remote_uri_length %> Int32.to_int)

let buffer_unpacker field_unpacker = 
    (fun (buf:Cstruct.buffer) -> field_unpacker (Cstruct.of_bigarray buf))

let user_get_created_on = buffer_unpacker get_user_created_on 
let user_get_last_update_time = buffer_unpacker get_user_last_update_time
let user_get_update_interval = buffer_unpacker get_user_update_interval

let user_get_type buf = 
    Cstruct.of_bigarray buf
    |> get_user_user_type 
    |> int_to_user_type

let make_int32_arg_int f = (fun a v -> f a (Int32.of_int v))
(* unused
    let make_int64_arg_int f = (fun a v -> f a (Int64.of_int v))
    *)
let string_setter_int32 o l = string_setter 
    (make_int32_arg_int o)
    (make_int32_arg_int l)

let user_with_name = string_setter_int32 set_user_name_offset set_user_name_length 
let user_with_bio = string_setter_int32 set_user_bio_offset set_user_bio_length 
let user_with_signature = string_setter_int32 set_user_signature_offset set_user_signature_length 
let user_with_remote_uri = string_setter_int32 set_user_remote_uri_length set_user_remote_uri_offset


type user = {
    password_hash : string option;
    bio : string option;
    signature : string option;
    avatar_id : int64 option;
    most_recent_post_id : int64 option;
    remote_uri : string option;
    update_interval : int64 option;
    name : string;
    user_type : user_type;
}

let default_user = {
    password_hash = None;
    bio = None;
    signature = None;
    avatar_id = None;
    most_recent_post_id = None;
    remote_uri = None;
    update_interval = None;
    name = "";
    user_type = LOCAL;
}

let create_user_buffer user : Cstruct.buffer =

    let res = buffer_builder_create sizeof_user in 
    let res = user_with_name res user.name in 
    let res = match user.bio with | Some bio -> user_with_bio res bio | None -> res in 
    let res = match user.signature with | Some s -> user_with_signature res s | None -> res in 
    let res = match user.remote_uri with | Some u -> user_with_remote_uri res u | None -> res in 

    let st = res.st in (
        (
            set_user_user_type st (user_type_to_int user.user_type);
            set_user_version st user_version;
            match user.avatar_id with | None -> () | Some v -> set_user_avatar_id st v;
            match user.most_recent_post_id with 
            | None -> ()
            | Some v -> set_user_most_recent_post_id st v;
            match user.update_interval with 
            | None -> () 
            | Some v -> set_user_update_interval st v;
            match user.password_hash with 
            | Some h -> set_user_password_hash h 0 st
            | None -> ()
        );

        buffer_builder_pack res
    )

[%%cstruct 
type attachment = {
    kind: uint8_t;
    offset: uint64_t;
    size: uint32_t;
}
[@@little_endian]]

type attachment_record = {
    kind: attachment_type;
    id: int;
    size: int
}

let default_attachment:attachment_record = {
    kind = BLOB;
    id = 0;
    size = 0;
}

[%%cstruct 
type post = {
    created_on: uint64_t;

    author_offset: uint32_t;
    author_size: uint32_t;

    content_offset: uint32_t;
    content_size: uint32_t;
    content_type: uint8_t;

    attachments_offset: uint32_t;
    n_attachments: uint32_t;

    prev_post_by_author: uint64_t;

    (* topics are arrays of uint32 id's and prev post pointers *)    
    n_topics: uint16_t;
    topics_offset: uint32_t;

    in_reply_to: uint64_t;
    next_reply: uint64_t;

    next_revision: uint64_t;
    prev_revision: uint64_t;

    deleted_on: uint64_t;

}
[@@little_endian]]

type post = {
    created_on : int64;
    author : string;
    content : string option;
    attachments : attachment_record array;
    prev_post_by_author : int64 option;
    topics : int32 array;
    in_reply_to : int64 option;
    deleted_on : int64 option;
}

let post_get_author = string_getter
    (get_post_author_offset %> Int32.to_int)
    (get_post_author_size %> Int32.to_int)

let post_get_content = string_getter
    (get_post_content_offset %> Int32.to_int)
    (get_post_content_size %> Int32.to_int)

let post_get_attachments blob = 
    let st = Cstruct.of_bigarray blob in
    let offset = get_post_attachments_offset st |> Int32.to_int in 
    let n_attachments = get_post_n_attachments st |> Int32.to_int in 
    if n_attachments < 1 then [| |] else 
        let res = Array.make n_attachments default_attachment in 
        let rec collect_attachments idx off = 
            if idx >= n_attachments then
                ()
            else (
                let attachment_st = Cstruct.of_bigarray
                    ~off:off ~len:sizeof_attachment blob in 
                let attachment = {
                    kind = get_attachment_kind attachment_st |> decode_attachment_type;
                    id = get_attachment_offset attachment_st |> Int64.to_int;
                    size = get_attachment_size attachment_st |> Int32.to_int;
                } in (
                    Array.set res idx attachment;
                    collect_attachments (idx + 1) (off + sizeof_attachment)
                )
            ) in 
        collect_attachments 0 offset; res

let post_get_topics = array_getter
    (get_post_topics_offset %> Int32.to_int)
    get_post_n_topics
    Int32.zero 4 LittleEndian.get_int32

let post_with_author = string_setter 
    (make_int32_arg_int set_post_author_offset)
    (make_int32_arg_int set_post_author_size)
let post_with_content = string_setter 
    (make_int32_arg_int set_post_content_offset)
    (make_int32_arg_int set_post_content_size)
let post_with_topics = array_setter 
    (make_int32_arg_int set_post_topics_offset)
    set_post_n_topics
    4 LittleEndian.set_int32

let post_get_created_on = buffer_unpacker get_post_created_on 

let post_with_attachments buf attachments = 
    let n_attachments = Array.length attachments in 
    if n_attachments < 1 then buf else 
    let res = Bigstring.create (n_attachments * sizeof_attachment) in (
        for i = 0 to n_attachments do
            let a_inp = Array.get attachments i in 
            let attachment = Cstruct.of_bigarray 
                ~off:(i * sizeof_attachment)
                ~len:sizeof_attachment res in (
                    set_attachment_kind attachment (attachment_type_to_int a_inp.kind);
                    set_attachment_offset attachment (Int64.of_int a_inp.id);
                    set_attachment_size attachment (Int32.of_int a_inp.size);
                )
        done;
        set_post_n_attachments buf.st (Int32.of_int n_attachments);
        set_post_n_attachments buf.st (Int32.of_int buf.size);
        {
            st = buf.st;
            size = buf.size + (Bigstring.length res);
            buffers = res :: buf.buffers;
        }
    )

let create_post_buffer post : Bigstring.t = 
    let res = buffer_builder_create sizeof_post in 
    let res = post_with_author res post.author in 
    let res = match post.content with | None -> res | Some v -> post_with_content res v in 
    let res = post_with_topics res post.topics in 
    let res = post_with_attachments res post.attachments in (
        set_post_created_on res.st post.created_on;
        (match post.prev_post_by_author with 
        | None -> ()
        | Some v -> set_post_prev_post_by_author res.st v);
        (match post.in_reply_to with 
        | None -> () 
        | Some v -> set_post_in_reply_to res.st v);
        (match post.deleted_on with 
        | None -> () 
        | Some v -> set_post_deleted_on res.st v);

        buffer_builder_pack res
    )