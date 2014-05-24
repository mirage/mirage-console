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

module Gnttab = Gnt.Gnttab

type t = {
  id: string;
  backend_id: int;
  gnt: Gnt.gntref;
  ring: Cstruct.t;
  mutable evtchn: Eventchn.t;
  waiters: unit Lwt.u Lwt_sequence.t;
}

type 'a io = 'a Lwt.t

type error = [ `Invalid_console of string ]
type id = string
exception Internal_error of string

let h = Eventchn.init ()
let id { id } = id

let connect id =
  let backend_id = 0 in
  let gnt = Gnt.console in
  let page = Start_info.console_start_page () in
  let ring = Io_page.to_cstruct page in
  Console_ring.Ring.init ring; (* explicitly zero the ring *)
  let get_evtchn () =
    let e = Eventchn.of_int Start_info.((get ()).console_evtchn) in
    Eventchn.unmask h e;
    e
  in
  let evtchn = get_evtchn () in
  let waiters = Lwt_sequence.create () in
  let cons = { id; backend_id; gnt; ring; evtchn; waiters } in
  Sched.add_resume_hook (fun () -> cons.evtchn <- get_evtchn (); Lwt.return ());
  Eventchn.unmask h evtchn;
  Eventchn.notify h evtchn;
  return (`Ok cons)

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
