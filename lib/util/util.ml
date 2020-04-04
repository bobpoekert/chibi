
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