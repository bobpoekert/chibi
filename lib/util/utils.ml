
let timestamp64 () = 
    Unix.time () 
    |> Int64.of_float

let cachev f = 
    let res = ref None in 
    fun () -> 
        match !res with 
        | Some v -> v 
        | None -> let v = f () in (
            res := Some v;
            v
        )

(* this returns a stream that you can use as a cohttp response body with Cohttp.Body.of_stream *)
let stream_of_file_slice ?buffer_size fd offset length = 
    let buffer_size = match buffer_size with | None -> 4096 | Some v -> v in
    let buffer = Bytes.create buffer_size in 
    let start_offset = offset in 
    let offset = ref offset in

    Lwt_stream.from (fun () ->
        let init_offset = !offset in 
        let length_read = init_offset - start_offset in 
        let length_left = length - length_read in 
        if length_left < 1 then Lwt.return None else 
        let%lwt bytes_read = Lwt_unix.read fd buffer init_offset (min length_left buffer_size) in (
            offset := init_offset + bytes_read;
            Lwt.return (Some 
            (
                if bytes_read < buffer_size then 
                    Bytes.sub_string buffer 0 bytes_read
                else
                    Bytes.to_string buffer
            ))
        )
    )

let crlf_re = Str.regexp "[\r\n]+"
let sse_stream instream = 
    Lwt_stream.map (fun (tag, s) -> 
        Printf.sprintf "tag: %s\r\nevent: %s\r\n\r\n" tag (
            Str.global_replace crlf_re "\\1data: " s
        )
    ) instream

let int64_to_base64 i = 
    let s = Bytes.create 8 in (
        EndianBytes.LittleEndian.set_int64 s 0 i;
        BatBase64.str_encode (Bytes.to_string s)
    )

let int64_of_base64 s = 
    let s = BatBase64.str_decode s |> Bytes.of_string in 
    EndianBytes.LittleEndian.get_int64 s 0


let encode_signed_value secret name value = 
    let timestamp = Unix.time () |> int_of_float in 
    let to_sign = Printf.sprintf "%d|%s|%s|" timestamp name value in 
    let signature =
        Nocrypto.Hash.SHA256.hmac ~key:secret (Cstruct.of_string to_sign)
        |> Cstruct.to_string 
        |> BatBase64.str_encode in 
    to_sign ^ signature

let decode_signed_value secret inp = 
    let inp_len = String.length inp in 
    let rec get_last_pipe start = 
        if start >= inp_len then start else 
        match String.index_from_opt inp start '|' with 
        | None -> start 
        | Some next_idx -> get_last_pipe next_idx in 
    let last_pipe_idx = get_last_pipe 0 in 
    let to_sign = String.sub inp 0 last_pipe_idx in 
    let signature = String.sub inp last_pipe_idx inp_len
        |> BatBase64.str_decode
        |> Cstruct.of_string in 
    let correct_signature = Nocrypto.Hash.SHA256.hmac ~key:secret (Cstruct.of_string to_sign) in 
    if signature != correct_signature then raise (Failure "bad signature") else 
    match String.split_on_char '|' inp with 
    | [_timestamp; name; value] -> (name, value) 
    | _ -> raise (Failure "malformed input")
