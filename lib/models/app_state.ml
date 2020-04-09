open Chibi_util
open EndianBigstring

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

let marshal_conv : config_type Lmdb.Conv.t = Lmdb.Conv.make 
    ~serialise:(fun alloc v -> 
        let bytes = Marshal.to_bytes v [] in 
        let size = Bytes.length bytes in 
        let res = alloc size in (
            Bigstring.blit_of_bytes bytes 0 res 0 size;
            res
        )
    )
    ~deserialise:(fun buf -> 
        let bytes = Bigstring.to_bytes buf in 
        Marshal.from_bytes bytes 0
    ) ()

let offset_conv = Lmdb.Conv.make 
    ~serialise:(fun alloc v -> 
        let res = alloc 8 in 
        let _ = EndianBigstring.LittleEndian.set_int64 res 0 v in 
        res
    )
    ~deserialise:(fun v -> EndianBigstring.LittleEndian.get_int64 v 0)
    ()

let config_defaults = let v = [
    ("max_log_chunk_size", Int (1024 * 1024 * 4));
    ("app_name", String "chibi")
] in List.fold_left (fun m (k, v) -> BatMap.add k v m) BatMap.empty v

let inner_get_config_param table k = 
    try 
        Lmdb.Map.get table k 
    with Not_found -> BatMap.find k config_defaults


let make_config_table db = 
    Lmdb.Map.create Lmdb.Map.Nodup
        ~name:"config" ~key:Lmdb.Conv.string ~value:marshal_conv db

let make_offsets_table db = 
    Lmdb.Map.create Lmdb.Map.Nodup
        ~name:"offsets" ~key:Lmdb.Conv.string ~value:offset_conv db


let init root_dir = 
    let db_dir = Format.sprintf "%s/%s" root_dir "db" in 
    let db_env = Lmdb.Env.create Lmdb.Rw db_dir in 
    let conf = make_config_table db_env in
    let max_chunk_size = match inner_get_config_param conf "max_log_chunk_size" with 
        | Int v -> v 
        | _ -> raise (Failure "max_log_chunk_size is wrong type") in 
    let log_dir = Format.sprintf "%s/%s" root_dir "posts" in 
    let log = Log.open_or_create_dir log_dir max_chunk_size in 
    let attachment_dir = Format.sprintf "%s/%s" root_dir "attachments" in
    let attachment_log = Log.open_or_create_dir attachment_dir max_chunk_size in 
    let offsets = make_offsets_table db_env in
    {
        db = db_env;
        config_data = conf;
        post_log = log;
        attachment_log = attachment_log;
        offsets = offsets;
    }

let state = init (Sys.getcwd ())

let read_attachment id = 
    let length = Log.read state.attachment_log id 4 in 
    let length = LittleEndian.get_int32 length 0 |> Int32.to_int in 
    Log.read state.attachment_log (id + 4) length

let add_attachment data = 
    let size = Bigstring.length data in 
    let blob = Bigstring.create (size + 4) in (
        LittleEndian.set_int32 blob 0 (Int32.of_int size);
        Bigstring.blit data 0 blob 0 (size + 4);
        Log.append state.attachment_log blob
    )

let get_offset k = Lmdb.Map.get state.offsets k
let set_offset k v = Lmdb.Map.set state.offsets k v

let get_config_param = inner_get_config_param state.config_data 
let set_config_param k v = Lmdb.Map.set state.config_data k v