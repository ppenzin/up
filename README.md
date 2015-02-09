up
==

[![Build Status](https://travis-ci.org/ppenzin/up.svg?branch=master)](https://travis-ci.org/ppenzin/up)

Software upgrader for FreeBSD, a frontend for portsnap,
portmaster/portupgrade/makefile. Tries to be smart by only upgrading software
that doesn't have any mentions in UPDATING.

# TODO
- Support installation mechanisms other than portmaster
- Fetching port tree: enable first-time runs (fetch-extract)

# Prerequisites
- ghc
- cabal

# Building
- cabal configure
- cabal build

Configure is only necessary at the first build.
