#!/bin/bash
set -e

NAME="checkml"
export OPAMYES=1

NCPU="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
OCAML_VERSION="5.1.0"
OPAM_SWITCH="$NAME-$OCAML_VERSION"

switch_exists=no
for installed_switch in $(opam switch list --short); do
  if [[ "$installed_switch" == "$OPAM_SWITCH" ]]; then
    switch_exists=yes
    break
  fi
done

if [ "$switch_exists" = "no" ]; then
  opam switch create $OPAM_SWITCH $OCAML_VERSION
else
  opam switch $OPAM_SWITCH
fi

eval $(SHELL=bash opam config env --switch=$OPAM_SWITCH)
# essential packages
opam pin https://github.com/prosyslab/logger.git
opam install -j $NCPU logger ocamlformat.0.26.0 merlin ocp-index ocp-indent ocaml-lsp-server ppx_compare ppx_deriving

# install tree sitter
git submodule update --init
pushd ocaml-tree-sitter-semgrep
make update
make setup
make
make install
pushd lang
./test-lang ocaml
pushd ocaml
make && make install
popd
popd
popd

# build
make
