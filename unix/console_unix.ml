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

(* TODO everything connects to the same console for now *)
(* TODO management service for logging *)
type t = {
  id: string;
  read_buffer: Cstruct.t;
  mutable closed: bool;
}

type id = string
type 'a io = 'a Lwt.t
type error = [ `Invalid_console of string ]
type buffer = Cstruct.t

let error_message (`Invalid_console msg) =
  Printf.sprintf "Invalid console '%s'" msg

let connect id =
  let read_buffer = Cstruct.create 1024 in
  let closed = false in
  let t = { id; read_buffer; closed } in
  return t

let disconnect _t = return ()
let id {id} = id

let read t =
  Lwt_bytes.read Lwt_unix.stdin t.read_buffer.Cstruct.buffer 0 (Cstruct.len t.read_buffer) >>= fun n ->
  if n = 0 || t.closed
  then return `Eof
  else return (`Ok (Cstruct.sub t.read_buffer 0 n))

let write_one t buf =
  Lwt_cstruct.complete
    (fun frag ->
       let open Cstruct in
       Lwt_bytes.write Lwt_unix.stdout frag.buffer frag.off frag.len
    ) buf

let write t buf =
  if t.closed then return `Eof
  else
    write_one t buf
    >>= fun () ->
    return (`Ok ())

let writev t bufs =
  if t.closed then return `Eof
  else
    Lwt_list.iter_s (write_one t) bufs
    >>= fun () ->
    return (`Ok ())

let close t =
  t.closed <- true;
  return ()

let write_string t buf off len = prerr_string (String.sub buf off len); flush stderr; len

let log t s = prerr_endline s

let log_s t s =
  let s = s ^ "\n" in
  let buf = Cstruct.create (String.length s) in
  Cstruct.blit_from_string s 0 buf 0 (String.length s);
  write_one t buf
