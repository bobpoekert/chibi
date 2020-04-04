
type 'a channel = ('a Lwt.t Queue.t * 'a Lwt.u option ref)
type ('k, 'a) bus = ('k, (int, 'a channel) Hashtbl.t) Hashtbl.t

let channel_make () : 'a channel = 
    let q = Queue.create () in 
    let recv, snd = Lwt.task () in (
        Queue.add recv q;
        (q, ref (Some snd))
    )

let channel_send (chan:'a channel) (v:'a) = 
    let q, snd = chan in 
    match !snd with 
    | None -> false
    | Some sndv -> (
        let new_recv, new_snd = Lwt.task () in (
            Queue.add new_recv q;
            snd := (Some new_snd);
            Lwt.wakeup_later sndv v;
            true
        )
    )

let channel_close (chan:'a channel) = 
    let _, chan = chan in 
    chan := None

(* getting the next element of the stream should not happen until the previous one delivers,
   and we start with one promise in the queue,
   and we add a new promise whenever we deliver a promise,
   so there should always be at least one promise in the queue unless the channel is closed
*)
let channel_stream (chan:'a channel) : 'a Lwt_stream.t = 
    let q, snd = chan in 
    Lwt_stream.from (fun () -> 
        match !snd with 
        | None -> Lwt.return None 
        | Some _ -> match Queue.take_opt q with 
            | None -> Lwt.return None 
            | Some recv -> let%lwt v = recv in Lwt.return (Some v)
    )

let subscription_id_ctr = ref 0

let bus_subscribe (b:('k, 'a) bus) (key:'k) = 
    let conn_set = match Hashtbl.find_opt b key with
    | Some v -> v 
    | None -> (let v = Hashtbl.create 100 in 
        Hashtbl.add b key v;
        v
    ) in 
    let _ = subscription_id_ctr := !subscription_id_ctr + 1 in
    let sub_id = !subscription_id_ctr in
    let chan = channel_make () in (
        Hashtbl.add conn_set sub_id chan;
        (sub_id, (channel_stream chan))
    )

let bus_unsubscribe (b:('k, 'a) bus) (key:'k) id = 
    match Hashtbl.find_opt b key with 
    | None -> ()
    | Some b -> Hashtbl.remove b id

let bus_publish (b:('k, 'a) bus) (key:'k) (v:'a) = 
    match Hashtbl.find_opt b key with 
    | None -> () 
    | Some b -> Hashtbl.iter (fun _k chan -> let _ = channel_send chan v in ()) b