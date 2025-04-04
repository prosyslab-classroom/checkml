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

eval $(SHELL=bash opam config env --switch=$OPAM_SWITCH --set-switch)
opam install -j $NCPU dune

# build
make
