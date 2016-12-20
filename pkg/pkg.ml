#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let metas = [
  Pkg.meta_file ~install:false "pkg/META";
  Pkg.meta_file ~install:false "pkg/META.lwt";
  Pkg.meta_file ~install:false "pkg/META.unix";
  Pkg.meta_file ~install:false "pkg/META.xen";
  Pkg.meta_file ~install:false "pkg/META.xen-proto";
  Pkg.meta_file ~install:false "pkg/META.xen-backend";
]

let opams =
  let opam_file = Pkg.opam_file ~lint_deps_excluding:None ~install:false in
  [
    opam_file "mirage-console.opam";
    opam_file "mirage-console-lwt.opam";
    opam_file "mirage-console-unix.opam";
    opam_file "mirage-console-xen.opam";
    opam_file "mirage-console-xen-proto.opam";
    opam_file "mirage-console-xen-backend.opam";
    opam_file "mirage-console-xen-cli.opam";
  ]

let () =
  Pkg.describe ~metas ~opams "mirage-console" @@ fun c ->
  match Conf.pkg_name c with
  | "mirage-console" ->
    Ok [ Pkg.lib "pkg/META";
         Pkg.lib ~exts:Exts.interface "src/mirage_console" ]
  | "mirage-console-lwt" ->
    Ok [ Pkg.lib "pkg/META.lwt" ~dst:"META";
         Pkg.lib ~exts:Exts.interface "lwt/mirage_console_lwt" ]
  | "mirage-console-unix" ->
    Ok [ Pkg.lib "pkg/META.unix" ~dst:"META";
         Pkg.mllib "unix/mirage-console-unix.mllib";
         Pkg.test "lib_test/portable"; ]
  | "mirage-console-xen" ->
    Ok [ Pkg.lib "pkg/META.xen" ~dst:"META";
         Pkg.mllib "xen/mirage-console-xen.mllib"; ]
  | "mirage-console-xen-proto" ->
    Ok [ Pkg.lib "pkg/META.xen-proto" ~dst:"META";
         Pkg.mllib "xen/proto/mirage-console-xen-proto.mllib"; ]
  | "mirage-console-xen-backend" ->
    Ok [ Pkg.lib "pkg/META.xen-backend" ~dst:"META";
         Pkg.mllib "xen/backend/mirage-console-xen-backend.mllib"; ]
  | "mirage-console-xen-cli" ->
    Ok [ Pkg.bin "xen/cli/main" ~dst:"mirage-xen-console" ]
  | other ->
    R.error_msgf "unknown package name: %s" other
