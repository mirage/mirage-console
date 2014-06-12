(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Printf
open OS
open Gnt

type t = {
  id: int;
  backend_id: int;
  gnt: Gnt.gntref;
  ring: Cstruct.t;
  mutable evtchn: Eventchn.t;
}

type 'a io = 'a Lwt.t

type error = [ `Invalid_console of string ]
type id = string
exception Internal_error of string

let h = Eventchn.init ()
let id { id } = string_of_int id

(* There are 2 console setup protocols in use. For the first console (index 0)
   the xenstore frontend is in /local/domain/$d/console, and usually contains
   the mfn which xenconsoled can use map_foreign_range on (although there is now
   a default grant table entry for it as well). All subsequent consoles
   (index 1 ... n) are treated as hotpluggable devices where the frontend is
   in /local/domain/%d/device/console/%d. For these consoles the grant mechanism
   is always used. *)

(* Allocate a ring, given the vdev and backend domid *)
let alloc (num, domid) =
  let buf = Io_page.get 1 in

  let ring = Io_page.to_cstruct buf in
  Console_ring.Ring.init ring; (* explicitly zero the ring *)

  Gntshr.get () >>= fun gnt ->
  Gntshr.grant_access ~domid ~writable:true gnt buf;

  return (gnt, ring)

(* Plug a console where id > 0 *)
let plug id =
  ( try return (int_of_string id)
    with _ -> fail (Failure (Printf.sprintf "Console.plug: id '%s' is not an integer" id)) ) >>= fun id ->

  ( if id = 0
    then fail (Failure ("Console.plug: one does not simply hotplug console 0"))
    else return () ) >>= fun () ->

  let node = Printf.sprintf "device/console/%d/%s" id in

  Xs.make () >>= fun xs ->
  Xs.(immediate xs (fun h -> read h (node "backend-id"))) >>= fun backend_id ->
  ( try return (int_of_string backend_id)
    with _ -> fail (Failure (Printf.sprintf "Non-numerical %s (%s)" (node "backend-id") backend_id)) ) >>= fun backend_id ->
  Xs.(immediate xs (fun h -> read h (node "backend"))) >>= fun backend ->

  alloc (id, backend_id) >>= fun (gnt, ring) ->

  let evtchn = Eventchn.bind_unbound_port h backend_id in
  let port = Eventchn.to_int evtchn in
  let ring_info = Conproto.RingInfo.({event_channel = port; ref = Int32.of_int gnt }) in
  let info = [
    "state", Device_state.(to_string Connected)
  ] @ (Conproto.RingInfo.to_assoc_list ring_info) in
  Xs.(transaction xs (fun h ->
      Lwt_list.iter_s (fun (k, v) -> write h (node k) v) info
    )) >>= fun () ->
  Xs.(wait xs (fun h ->
      lwt state = read h (sprintf "%s/state" backend) in
      if Device_state.(of_string state = Connected) then return () else fail Xs_protocol.Eagain
    )) >>= fun () ->
  printf "Console %d: connected\n" id;

  Eventchn.unmask h evtchn;
  let t = { id; backend_id; gnt; ring; evtchn  } in

  return t

(** Return a list of available consoles *)
let enumerate () =
  Xs.make () >>= fun xs ->
  try_lwt
    Xs.(immediate xs (fun h -> directory h "device/console"))
  with
  | Xs_protocol.Enoent _ ->
    return []
  | e ->
    printf "Console.enumerate caught exception: %s\n" (Printexc.to_string e);
    return []

(* Return the name -> id mapping, where a single id has a numerical name and
   may also have a human-readable name *)
let names_to_ids ids =
  Xs.make () >>= fun xs ->
  Lwt_list.fold_left_s (fun list id ->
    try_lwt
      Xs.(immediate xs (fun h -> read h (Printf.sprintf "device/console/%s/name" id))) >>= fun n ->
      return ((n, id) :: (id, id) :: list)
    with Xs_protocol.Enoent path ->
      return ((id, id) :: list)
    | e ->
      printf "Console.names_to_ids while processing %s caught exception: %s, skipping\n" id (Printexc.to_string e);
      return list
  ) [] ids

let get_initial_console () =
  (* The domain is created with a reserved grant entry already set up.
     We don't need to know who the backend domain is. *)
  let backend_id = 0 in (* unused *)
  let gnt = Gnt.console in (* unused *)

  let page = Start_info.console_start_page () in
  let ring = Io_page.to_cstruct page in
  Console_ring.Ring.init ring; (* explicitly zero the ring *)

  let get_evtchn () =
    let e = Eventchn.of_int Start_info.((get ()).console_evtchn) in
    Eventchn.unmask h e;
    e in
  let evtchn = get_evtchn () in
  let cons = { id = 0; backend_id; gnt; ring; evtchn  } in
  Sched.add_resume_hook (fun () -> cons.evtchn <- get_evtchn (); Lwt.return ());

  Eventchn.unmask h evtchn;
  Eventchn.notify h evtchn;
  cons

let devices : (string, t) Hashtbl.t = Hashtbl.create 16

let connect id =
  if id = "0" || id = "" then begin
    let t = get_initial_console () in
    Hashtbl.replace devices id t;
    return (`Ok t)
  end else begin
    if Hashtbl.mem devices id then begin
      let d = Hashtbl.find devices id in
      return (`Ok d)
    end else begin
      enumerate () >>= fun all ->
      names_to_ids all >>= fun list ->
      if List.mem_assoc id list then begin
        let id' = List.assoc id list in
        printf "Console.connect %s: opening device %s\n" id id';
        plug id' >>= fun d ->
        let names = List.map fst (List.filter (fun (_, v) -> v = id') list) in
        List.iter (fun name -> Hashtbl.replace devices name d) names;
        return (`Ok d)
      end else begin
        printf "Connect.connect %s: could not find device\n" id;
        return (`Error (`Invalid_console
                        (Printf.sprintf "device %s not found (available = [ %s ])"
                           id (String.concat ", " all))))
      end
    end
  end

let disconnect _id =
  return ()

let rec write_all_low event cons buf off len =
  if len > String.length buf - off
  then Lwt.fail (Invalid_argument "len")
  else
    let w = Console_ring.Ring.Front.unsafe_write cons.ring buf off len in
    Eventchn.notify h cons.evtchn;
    let left = len - w in
    assert (left >= 0);
    if left = 0 then return ()
    else
      Activations.after cons.evtchn event
      >>= fun event ->
      write_all_low event cons buf (off+w) left

let write_all cons buf off len =
  write_all_low Activations.program_start cons buf off len

let write cons buf off len =
  if len > String.length buf - off then raise (Invalid_argument "len");
  let nb_written = Console_ring.Ring.Front.unsafe_write cons.ring buf off len in
  Eventchn.notify h cons.evtchn;
  nb_written

let read cons buf off len =
  if len > String.length buf - off then fail (Invalid_argument "len")
  else begin
    let nb_read = Console_ring.Ring.Front.unsafe_read cons.ring buf off len in
    Eventchn.notify h cons.evtchn;
    return nb_read
  end

let log t s =
  let s = s ^ "\r\n" in
  let (_:int) = write t s 0 (String.length s) in ()

let log_s t s =
  let s = s ^ "\r\n" in
  write_all_low Activations.program_start t s 0 (String.length s)
