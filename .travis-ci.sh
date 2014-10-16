XEN_OPAM_DEPENDS="xen-evtchn xen-gnt mirage-xen"
COMMON_OPAM_DEPENDS="lwt cstruct mirage io-page mirage-types xenstore xenstore_transport"

case "$XEN" in
1) OPAM_DEPENDS="$COMMON_OPAM_DEPENDS $XEN_OPAM_DEPENDS";;
*) OPAM_DEPENDS="$COMMON_OPAM_DEPENDS";;
esac

case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.2.0) ppa=avsm/ocaml312+opam12 ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.2.0) ppa=avsm/ocaml40+opam12 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
4.02.0,1.2.0) ppa=avsm/ocaml42+opam12 ;;
4.02.0,1.1.0) ppa=avsm/ocaml42+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam

export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam remote add mirage git://github.com/mirage/mirage-dev
depext=`opam install -e ubuntu $OPAM_DEPENDS`
sudo apt-get install -qq $depext
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
