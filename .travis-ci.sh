case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.2.0) ppa=avsm/ocaml312+opam12; pinx="add" ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.2.0) ppa=avsm/ocaml40+opam12; pinx="add" ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.2.0) ppa=avsm/ocaml41+opam12; pinx="add" ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
4.02.0,1.2.0) ppa=avsm/ocaml42+opam12; pinx="add" ;;
4.02.0,1.1.0) ppa=avsm/ocaml42+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam libxen-dev aspcud

export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam remote add mirage git://github.com/mirage/mirage-dev

opam pin $pinx mirage-console .

make
make test

opam remove mirage-console
opam install mirage-xen xenstore mirage-console -v

opam remove mirage-console mirage-xen xenstore
opam install shared-memory-ring xen-gnt xen-evtchn xenstore mirage-console xenstore_transport -v

opam remove mirage-xen mirage-console shared-memory-ring xen-gnt xen-evtchn xenstore xenstore_transport
opam install mirage-console -v
