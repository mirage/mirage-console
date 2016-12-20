.PHONY: unix xen-proto xen-cli xen-backend xen

sigs:
	ocaml pkg/pkg.ml build -n mirage-console -q
	ocaml pkg/pkg.ml build -n mirage-console-lwt -q

unix:
	ocaml pkg/pkg.ml build --pkg-name mirage-console-unix

test: unix
	ocaml pkg/pkg.ml test

xen-proto:
	ocaml pkg/pkg.ml build --pkg-name mirage-console-xen-proto

xen-cli:
	ocaml pkg/pkg.ml build --pkg-name mirage-console-xen-cli

xen-backend:
	ocaml pkg/pkg.ml build --pkg-name mirage-console-xen-backend

xen:
	ocaml pkg/pkg.ml build --pkg-name mirage-console-xen

clean:
	ocaml pkg/pkg.ml clean
	rm -f *.install
