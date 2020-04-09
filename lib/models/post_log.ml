open Crc
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
    Crc32.string (Bigstring.to_string data) 0 (Bigstring.length data)

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
    let checksum = Crc32.string (Bigstring.to_string blob) 0 length in 
    let res = Bigstring.create (length + sizeof_log_header) in 
    let header = Cstruct.of_bigarray ~off:length ~len:sizeof_log_header res in (
        Bigstring.blit blob 0 res 0 length;
        set_log_header_length header (Int64.of_int length);
        set_log_header_checksum header checksum;
        set_log_header_entry_type header (log_entry_type_to_int entry_type);
        set_log_header_magic_number header correct_magic_number;
        Log.append flog res
    )

let rec update_log_entry updater flog offset = 
    let header_offset = offset - sizeof_log_header in 
    let header_blob = Log.read flog header_offset sizeof_log_header in
    let header_st = Cstruct.of_bigarray header_blob in 
    let old_checksum = get_log_header_checksum header_st in
    let header_blob = Cstruct.of_bigarray header_blob in 
    let entry_length = get_log_header_length header_blob |> Int64.to_int in 
    let entry_start = header_offset - entry_length in 
    let entry_blob = Log.read flog entry_start entry_length in 
    let newval = updater entry_blob in 
    if (Bigstring.length newval) != entry_length then
        raise (Failure "log entries cannot change size")
    else
        let new_checksum = get_checksum newval in
        let _ = set_log_header_checksum header_blob new_checksum in 
        let write_buffer = Bigstring.create (entry_length + sizeof_log_header) in 
        let _ = Bigstring.blit newval 0 write_buffer 0 entry_length in 
        let _ = Bigstring.blit (Cstruct.to_bigarray header_blob) 0 write_buffer entry_length sizeof_log_header in 
        (* CAS loop in case we got beaten to the write *)
        Log.with_write_lock (fun () -> 
            let new_header_blob = Log.read flog header_offset sizeof_log_header in
            let new_header_st = Cstruct.of_bigarray new_header_blob in 
            let check_checksum = get_log_header_checksum new_header_st in 
            if check_checksum != old_checksum then
                (* CAS retry *)
                update_log_entry updater flog offset 
            else
                Log.overwrite flog write_buffer header_offset
        ) flog
