#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let metas = [
    Pkg.meta_file ~install:false "pkg/META.unix";
    Pkg.meta_file ~install:false "pkg/META.xen";
    Pkg.meta_file ~install:false "pkg/META.xen-proto";
    Pkg.meta_file ~install:false "pkg/META.xen-backend";
  ]

let opams =
  let lint_deps_excluding = None in
  let install = false in
  [
    Pkg.opam_file "mirage-console-unix.opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "mirage-console-xen.opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "mirage-console-xen-proto.opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "mirage-console-xen-backend.opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "mirage-console-xen-cli.opam" ~lint_deps_excluding ~install;
  ]

let () =
  Pkg.describe ~metas ~opams "mirage-console-unix" @@ fun c ->
  match Conf.pkg_name c with
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
