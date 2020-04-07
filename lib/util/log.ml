open EndianBigstring
open Bigarray
open BatPervasives

type mapped_file = {
    fd : Unix.file_descr;
    buffer : Bigstring.t
}

let map_file size fname : mapped_file = 
    let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT; Unix.O_APPEND] 0o660 in 
    let _ = Unix.ftruncate fd size in 
    let buffer = Unix.map_file fd Char c_layout true [| -1 |] in 
    let buffer = array1_of_genarray buffer in
    {fd = fd; buffer = buffer;}

let file_get_end_offset (f:mapped_file) = 
    LittleEndian.get_int32 f.buffer 0

let file_set_end_offset (f:mapped_file) offset = 
    LittleEndian.set_int32 f.buffer 0 offset

(* unused
let file_read (f:mapped_file) off len : Bigstring.t = 
    Array1.sub f.buffer (off + 4) len

let file_write (f:mapped_file) off (data:Bigstring.t) =
    let data_size = Bigstring.length data in 
    let data_end = off + data_size in 
    let old_off = file_get_end_offset f |> Int32.to_int in (
        Bigstring.blit data 0 f.buffer off data_size;
        if data_end > old_off then file_set_end_offset f (Int32.of_int data_end);
    )

    *)

type log_files = (int * mapped_file) list (* in reverse order, highest offset first *)

type log = {
    files: log_files ref;
    dir: string;
    write_lock: Mutex.t;
    max_file_size: int;
}


let log_default () = {
    files = ref [];
    dir = "";
    write_lock = Mutex.create();
    max_file_size = 0;
}

let log_file_open l fname = 
    (
        Utils.int64_of_base64 fname |> Int64.to_int,
        map_file l.max_file_size
            (Printf.sprintf "%s/%s" l.dir fname)
    )

let log_get_end_offset l = 
    let last_start, last_fd = List.hd !(l.files) in 
    let last_end = file_get_end_offset last_fd |> Int32.to_int in 
    last_start + last_end

(* unused 
let incr_log_end_offset l inc = 
    let _last_start, last_fd = BatList.last !(l.files) in 
    let last_end = file_get_end_offset last_fd |> Int32.to_int in 
    file_set_end_offset last_fd (Int32.of_int (last_end + inc))
    *)

let create_log_file l off = 
    let fname = Utils.int64_to_base64 (Int64.of_int off) in 
    let full_path = Format.sprintf "%s/%s" l.dir fname in 
    map_file l.max_file_size full_path

let append_log_file f = 
    let off = log_get_end_offset f in 
    let new_file = create_log_file f off in 
    f.files := (off, new_file) :: !(f.files);
    (off, new_file)

let create_dir l = 
    Unix.mkdir l.dir 0o660;
    l.files := [(0, create_log_file l 0)]

let rec list_of_dir dir res = 
    try 
        list_of_dir dir ((Unix.readdir dir) :: res)
    with End_of_file -> res

let listdir dirname = 
    let dir = Unix.opendir dirname in 
    let res = list_of_dir dir [] in 
    Unix.closedir dir; res

let open_dir l = 
    let files = List.map (log_file_open l) (listdir l.dir) in 
    match files with 
    | [] -> raise (Failure "log directory exists but is empty. this should be impossible.") 
    | files -> 
        l.files := List.sort (fun (a, _) (b, _) -> compare b a) files


let open_or_create_dir dirname max_file_size = 
    let res = {(log_default ()) with 
        max_file_size = max_file_size;
        dir = dirname;
    } in 
    if Sys.file_exists dirname then 
        open_dir res
    else
        create_dir res;
    res

let file_containing fs start = 
    let h_start, h_f = List.hd fs in 
    let h_end = file_get_end_offset h_f |> Int32.to_int in 
    let h_end = h_end + h_start in 
    if h_end < start then raise Not_found else
    List.find (fun (off, f) -> (off + (file_get_end_offset f |> Int32.to_int)) < start) fs

let read l start len = 
    let files = !(l.files) in 
    let off, fd = file_containing files start in 
    Bigstring.sub fd.buffer (start - off) len

let read_end l len = 
    let fs = !(l.files) in 
    let _last_off, last_f = List.hd fs in 
    let end_off = file_get_end_offset last_f |> Int32.to_int in 
    Bigstring.sub last_f.buffer (end_off - len) len

let with_write_lock f l = 
    let not_locked = Mutex.try_lock l.write_lock in
    if not not_locked then f () else 
    finally (fun () -> Mutex.unlock l.write_lock) f ()

let overwrite l buffer start = 
    with_write_lock (fun () ->
        let files = !(l.files) in 
        let (f_off, fd) = file_containing files start in 
        let fd_start = start - f_off in 
        Bigstring.blit buffer 0 fd.buffer fd_start (Bigstring.length buffer)
    ) l

let append l buffer = 
    let buffer_size = Bigstring.length buffer in 
    let rec run () = (
        let files = !(l.files) in 
        let _last_off, last_fd = List.hd files in 
        let last_file_size = file_get_end_offset last_fd |> Int32.to_int in 
        let new_end = last_file_size + buffer_size in 
        if new_end > l.max_file_size then 
            let _ = append_log_file l in run ()
        else (
            file_set_end_offset last_fd (Int32.of_int new_end);
            Bigstring.blit buffer 0 last_fd.buffer last_file_size buffer_size
        )
    ) in
    if buffer_size > l.max_file_size then
        raise (Failure "buffer too big, cannot append to file")
    else with_write_lock run l