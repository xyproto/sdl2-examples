#!/bin/sh
#
# csv.sh
#
# Measure approximately how long it takes to build each example,
# and how large the resulting executable is.
#
# The examples are (mostly) not size-optimized, so the sizes will vary
# a lot, without saying much about the programming language.
#
# For instance, Go produces large executables by default, but when using
# gccgo, -Os and upx, the executables are orders of magnitude smaller.
#

bench() {
  echo -n "$1;"
  pushd "$1" > /dev/null
  make -s clean > /dev/null
  echo -n $(time -f "%es" make -s 2>&1 | tail -1)
  echo -n ';'
  echo -n $(du -b "$2" | sed 's/[^0-9]//g')
  popd > /dev/null
  echo
}

cmakebench() {
  echo -n "$1;"
  pushd "$1" > /dev/null
  test -d build && rm -r build || true
  echo -n $(time -f "%es" cmake -S . -B build 2>&1 > /dev/null && make -C build 2>&1 >/dev/null | tr -d '\n')
  echo -n ';'
  echo -n $(du -b "build/$2" | sed 's/[^0-9]//g')
  popd > /dev/null
  echo
}

cxxbench() {
  echo -n "$1;"
  pushd "$1" > /dev/null
  cxx clean > /dev/null
  echo -n $(time -f "%es" cxx 2>&1 >/dev/null | tr -d '\n')
  echo -n ';'
  echo -n $(du -b "$2" | sed 's/[^0-9]//g')
  popd > /dev/null
  echo

}

stackbench() {
  echo -n "$1;"
  pushd "$1" > /dev/null
  stack clean > /dev/null
  echo -n $(time -f "%es" stack build 2>&1 | tail -1)
  echo -n ';'
  local filename=$(find . -executable -type f -name grumpycat-exe -printf "%T@ %Tc %p\n" | sort -n | tail -1 | rev | cut -d' ' -f1 | rev)
  echo -n $(du -b $filename) | tr ' ' '\n' | head -1 | tr -d '\n'
  popd > /dev/null
  echo
}

main() {
  echo 'language;compilation;bytesize'
  bench go go
  bench ada main
  for f in c89 c++98 c99 c11 c++11 c18 c2x; do
    bench "$f" main
  done
  for f in *-cmake; do
    cmakebench "$f" main
  done
  #cxxbench c++20-cxx c++20-cxx
  bench crystal bin/main
  bench csharp main
  bench d main
  bench objectivepascal main
  bench freebasic main
  stackbench haskell
  # TODO: Add the other languages as well, except for gccgo, mruby, lua and python
}

main
