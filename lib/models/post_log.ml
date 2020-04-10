open Chibi_util

(*

This is an implementation of an append-only log that acts as a database for feed data. 
New messages are added to the end of the log. 
Entries that have already been written to the log may be changed, but their size may not. 
In practice this means you can overwrite fixed-sized fields (eg: integers)
of structs in log entries but not variable-length fields. 

We use this log to store feed data, since there's a close correspondence between
a sequential log like this and the way you want the data actually displayed. 

Right now all log entries go into one big file though that might change in the future. 
If it does change it will work by naming each file with the byte offset into the larger
log that it starts at, so byte offsets can be kept consistent.

*)

[%%cenum 
type log_entry_type = 
| POST 
| LIKE 
| USER
[@@uint16_t]]

(* NOTE: if you move or remove any of these fields you break compatibility with existing databases *)
[%%cstruct 
type log_header = {
    magic_number: uint32_t; (* this is so we can find the last entry in the log *)
    checksum: uint32_t;
    entry_type: uint16_t;
    length: uint64_t;
}
[@@little_endian]]

type data_corrupt = 
| Bad_magic_number 
| Bad_checksum

exception Corrupt of data_corrupt

let correct_magic_number = EndianString.LittleEndian.get_int32 "FLOG" 0

let get_checksum data = 
    Crc.unsafe_crc32_cstruct 0l data 0 (Bigstring.length data)

let get_post_header l offset = 
    let header_offset = offset - sizeof_log_header in 
    let header_blob = Log.read l header_offset sizeof_log_header in 
    Cstruct.of_bigarray header_blob

let rec inner_read_log_entry offset l ctr = 
    if ctr > 10 then raise (Corrupt Bad_checksum) else
    let offset = match offset with 
        | Some o -> o 
        | None -> Log.end_off l in 
    let header_blob = get_post_header l offset in
    let header_offset = offset - sizeof_log_header in 

    let magic = get_log_header_magic_number header_blob in 
    if magic != correct_magic_number then raise (Corrupt Bad_magic_number) else
    let checksum = get_log_header_checksum header_blob in 
    let entry_type = get_log_header_entry_type header_blob |> int_to_log_entry_type in 
    let entry_length = get_log_header_length header_blob |> Int64.to_int in 
    let entry_blob = Log.read l (header_offset - entry_length) entry_length in 
    
    (* TODO checking the checksum on reads shouldn't be necessary. try commenting out these two lines *)
    let actual_checksum = get_checksum entry_blob in
    if checksum != actual_checksum then inner_read_log_entry (Some offset) l (ctr + 1) else 

    (entry_type, entry_blob)

let read_log_entry ?offset l = inner_read_log_entry offset l 0

let prev_entry_id l offset = 
    if offset < 1 then None else
    let header = get_post_header l offset in 
    let size = get_log_header_length header |> Int64.to_int in 
    Some (offset - size - sizeof_log_header)

let append_log_entry flog entry_type blob = 
    let length = Bigstring.length blob in 
    let checksum = get_checksum blob in 
    let res = Bigstring.create (length + sizeof_log_header) in 
    let header = Cstruct.of_bigarray ~off:length ~len:sizeof_log_header res in (
        Bigstring.blit blob 0 res 0 length;
        set_log_header_length header (Int64.of_int length);
        set_log_header_checksum header checksum;
        set_log_header_entry_type header (log_entry_type_to_int entry_type);
        set_log_header_magic_number header correct_magic_number;
        Log.append flog res
    )

let update_locked_log_entry_inplace updater flog offset = 
    let header_offset = offset - sizeof_log_header in 
    let header_blob = Log.read flog header_offset sizeof_log_header in
    let header_st = Cstruct.of_bigarray header_blob in 
    let body_size = get_log_header_length header_st |> Int64.to_int in 
    let whole_entry = Bigstring.sub (Log.get_buffer flog)
        (offset  - sizeof_log_header - body_size)
        (body_size + sizeof_log_header) in
    let body = Bigstring.sub whole_entry 0 (body_size - sizeof_log_header) in
    updater header_st body;
    set_log_header_checksum header_st (get_checksum body)


