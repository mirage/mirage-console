(*
 * Copyright (C) Citrix Systems Inc.
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

let project_url = "http://github.com/mirage/mirage-console"

open Lwt
open Conproto
open Xs_protocol
module Client = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
open Client

module Common = struct
  type t = {
    verbose: bool;
    debug: bool;
  }
  (** options common to all subcommands *)

  let make verbose debug = { verbose; debug }
end

let ( >>= ) = Conproto.( >>= )

let logger = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

let read_one client k = immediate client (fun xs ->
  try_lwt
    lwt v = read xs k in
    return (`OK v)
  with _ -> return (`Error ("failed to read: " ^ k)))

let exists client k = match_lwt read_one client k with `Error _ -> return false | _ -> return true

let find_vm client vm =
  (* First interpret as a domain ID, then UUID, then name *)
  let domainpath x = "/local/domain/" ^ x in
  lwt e = exists client (domainpath vm) in
  if e
  then return (Some (int_of_string vm))
  else begin
    lwt valid_domids = immediate client (fun xs -> directory xs "/local/domain") in
    lwt valid_uuids = Lwt_list.map_s (fun d ->
      match_lwt read_one client ("/local/domain/" ^ d ^ "/vm") with
      | `OK path -> return (Some (Filename.basename path))
      | `Error _ -> return None
    ) valid_domids in
    lwt valid_names = Lwt_list.map_s (fun d ->
      match_lwt read_one client ("/local/domain/" ^ d ^ "/name") with
      | `OK path -> return (Some path)
      | `Error _ -> return None
    ) valid_domids in
    let uuids_to_domids = List.combine valid_uuids valid_domids in
    let names_to_domids = List.combine valid_names valid_domids in
    if List.mem_assoc (Some vm) uuids_to_domids
    then return (Some (int_of_string (List.assoc (Some vm) uuids_to_domids)))
    else if List.mem_assoc (Some vm) names_to_domids
    then return (Some (int_of_string (List.assoc (Some vm) names_to_domids)))
    else return None
  end

let (|>) a b = b a
let find_free_console client vm =
  lwt used = immediate client (fun xs -> try_lwt directory xs (Printf.sprintf "/local/domain/%d/device/console" vm) with Xs_protocol.Enoent _ -> return []) in
  let free =
    used
    |> List.map int_of_string
    |> List.fold_left max 0 (* console 0 is handled specially *)
    |> (fun x -> x + 1) in
  return free

let main (vm: string) =
  lwt client = make () in
  (* Figure out where the device is going to go: *)
  lwt vm = match_lwt find_vm client vm with
    | Some vm -> return vm
    | None -> fail (Failure (Printf.sprintf "Failed to find VM %s" vm)) in
  Printf.fprintf stderr "Operating on VM domain id: %d\n%!" vm;
  lwt devid = find_free_console client vm in
  Printf.fprintf stderr "Creating device %d (linux device /dev/tty%d)\n%!"
    devid (devid);

  let c = Connection.make ~frontend_domid:vm ~virtual_device:devid () in
  transaction client (fun xs ->
    Lwt_list.iter_s (fun (acl, (k, v)) ->
      lwt () = write xs k v in
      lwt () = setperms xs k acl in
      return ()
    ) (Connection.to_assoc_list c)
  )

let connect (common: Common.t) (vm: string) =
  match vm with
    | "" ->
      `Error(true, "I don't know which VM to operate on. Please supply a VM name or uuid.")
    | vm ->
      let () = Lwt_main.run (main vm) in
      `Ok ()

open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [
 `S _common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Options common to all commands *)
let common_options_t =
  let docs = _common_options in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [false] [verbose]) in
  Term.(pure Common.make $ debug $ verb)

let connect_command =
  let doc = "Connect a console to a specific VM." in
  let man = [
    `S "DESCRIPTION";
    `P "Connect a console to a specific VM.";
  ] in
  let vm =
    let doc = "The domain, UUID or name of the VM to connect to." in
    Arg.(required & pos 0 (some string) None & info [ ] ~docv:"VM-name-or-uuid" ~doc) in
  Term.(ret (pure connect $ common_options_t $ vm)),
  Term.info "connect" ~sdocs:_common_options ~doc ~man

let default_cmd =
  let doc = "manipulate virtual console devices on Xen virtual machines" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "mirage-console" ~version:"0.1" ~sdocs:_common_options ~doc ~man

let cmds = [ connect_command ]

let _ =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
