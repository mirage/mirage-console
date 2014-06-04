.PHONY: all clean install build
all: build doc

J=4

ENABLE_MIRAGE_XEN ?= $(shell if ocamlfind query mirage-xen >/dev/null 2>&1; then echo --enable-miragexen; else echo --disable-miragexen; fi)

ENABLE_XEN ?= $(shell if ocamlfind query xen-gnt >/dev/null 2>&1; then echo --enable-xen; else echo --disable-xen; fi)

export OCAMLRUNPARAM=b

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure $(ENABLE_MIRAGE_XEN) $(ENABLE_XEN) --enable-tests

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

install: setup.bin
	@./setup.bin -install

uninstall:
	@./setup.bin -uninstall

test: setup.bin build
	./setup.bin -test

reinstall: setup.bin
	@./setup.bin -reinstall

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
