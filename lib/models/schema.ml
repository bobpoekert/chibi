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
    liked_post_ids_offset: uint32_t;
    n_liked_post_ids: uint64_t;
    most_recent_post_id: uint64_t;
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

let buffer_builder_append builder buffer = {
    st = builder.st;
    buffers = buffer :: builder.buffers;
    size = (Array1.dim buffer) + builder.size;
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

let array_setter offset_setter length_setter int_width packer = 
    fun builder vs -> 
        let length = Array.length vs in
        let res = Bigstring.create (int_width * length) in 
        let new_off = builder.size + (length * int_width) in
        for idx = 0 to length do 
            packer res (idx * int_width) (Array.get vs idx)
        done;
        offset_setter builder.st new_off;
        length_setter builder.st length;
        {
            st = builder.st;
            buffers = res :: builder.buffers;
            size = new_off;
        }

let get_user_name = varchar_getter 
    (get_user_name_offset %> Int32.to_int)
    (get_user_name_length %> Int32.to_int)
let get_user_bio = varchar_getter
    (get_user_bio_offset %> Int32.to_int)
    (get_user_bio_length %> Int32.to_int)
let get_user_signature = varchar_getter 
    (get_user_signature_offset %> Int32.to_int)
    (get_user_signature_length %> Int32.to_int)
let get_user_remote_uri = varchar_getter
    (get_user_remote_uri_offset %> Int32.to_int)
    (get_user_remote_uri_length %> Int32.to_int)

let get_user_liked_post_ids = array_getter
    (get_user_liked_post_ids_offset %> Int32.to_int) 
    (get_user_n_liked_post_ids %> Int64.to_int)
    Int64.zero 8 LittleEndian.get_int64

let make_int32_arg_int f = (fun a v -> f a (Int32.of_int v))
let make_int64_arg_int f = (fun a v -> f a (Int64.of_int v))
let string_setter_int32 o l = string_setter 
    (make_int32_arg_int o)
    (make_int32_arg_int l)

let user_with_name = string_setter_int32 set_user_name_offset set_user_name_length 
let user_with_bio = string_setter_int32 set_user_bio_offset set_user_bio_length 
let user_with_signature = string_setter_int32 set_user_signature_offset set_user_signature_length 
let user_with_remote_uri = string_setter_int32 set_user_remote_uri_length set_user_remote_uri_offset

let user_with_liked_post_ids = array_setter 
    (make_int32_arg_int set_user_liked_post_ids_offset)
    (make_int64_arg_int set_user_n_liked_post_ids)
     8 LittleEndian.set_int64

let maybe_string_length s = 
    match s with 
    | Some s -> String.length s 
    | None -> 0

let create_user_buffer
    ?password_hash 
    ?bio 
    ?signature 
    ?avatar_id
    ?liked_post_ids
    ?most_recent_post_id
    ?remote_uri 
    ?update_interval
    id name user_type : Bigstring.t =

    let res = buffer_builder_create sizeof_user in 
    let res = user_with_name res name in 
    let res = match bio with | Some bio -> user_with_bio res bio | None -> res in 
    let res = match signature with | Some s -> user_with_signature res s | None -> res in 
    let res = match remote_uri with | Some u -> user_with_remote_uri res u | None -> res in 
    let res = match liked_post_ids with
        | Some i -> user_with_liked_post_ids res (Array.map Int64.of_int i)
        | None -> res in

    let st = res.st in (
        (
            set_user_id st id;
            set_user_user_type st (user_type_to_int user_type);
            match avatar_id with | None -> () | Some v -> set_user_avatar_id st v;
            match most_recent_post_id with 
            | None -> ()
            | Some v -> set_user_most_recent_post_id st v;
            match update_interval with 
            | None -> () 
            | Some v -> set_user_update_interval st v;
            match password_hash with 
            | Some h -> set_user_password_hash h 0 st
            | None -> ()
        );

        buffer_builder_pack res
    )

[%%cstruct 
type attachment = {
    kind: uint8_t;
    offset: uint64_t;
    size: uint64_t;
}
[@@little_endian]]

type attachment_record = {
    kind: attachment_type;
    offset: int;
    size: int
}

let default_attachment:attachment_record = {
    kind = BLOB;
    offset = 0;
    size = 0;
}

[%%cstruct 
type post = {
    author_id: uint64_t;
    created_on: uint64_t;

    content_offset: uint32_t;
    content_size: uint32_t;
    content_type: uint8_t;

    attachments_offset: uint32_t;
    n_attachments: uint32_t;

    prev_post_by_author: uint64_t;

    (* topics are an array of uint32 id's *)    
    n_topics: uint16_t;
    topics_offset: uint32_t;

    in_reply_to: uint64_t;
    next_reply: uint64_t;

    next_revision: uint64_t;
    prev_revision: uint64_t;

    deleted_on: uint64_t;

}
[@@little_endian]]

let get_post_content = string_getter
    (get_post_content_offset %> Int32.to_int)
    (get_post_content_size %> Int32.to_int)

let get_post_attachments blob = 
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
                    offset = get_attachment_offset attachment_st |> Int64.to_int;
                    size = get_attachment_size attachment_st |> Int64.to_int;
                } in (
                    Array.set res idx attachment;
                    collect_attachments (idx + 1) (off + sizeof_attachment)
                )
            ) in 
        collect_attachments 0 offset; res

let get_post_topics = array_getter
    (get_post_topics_offset %> Int32.to_int)
    get_post_n_topics
    Int32.zero 4 LittleEndian.get_int32

let set_post_content = string_setter 
    (make_int32_arg_int set_post_content_offset)
    (make_int32_arg_int set_post_content_size)
let post_with_topics = array_setter 
    (make_int32_arg_int set_post_topics_offset)
    set_post_n_topics
    4 LittleEndian.set_int32
