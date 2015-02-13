up
==

[![Build Status](https://travis-ci.org/ppenzin/up.svg?branch=master)](https://travis-ci.org/ppenzin/up)

Software upgrader for FreeBSD, a frontend for portsnap,
portmaster/portupgrade/makefile. Tries to be smart by only upgrading software
that doesn't have any mentions in UPDATING.

# Prerequisites
- ghc
- cabal

# Building
- cabal configure
- cabal build

Configure is only necessary at the first build.

# TODO (aka Future Features)
## Parse UPDATING
Checking whether port needs any manual intervention is done by simply grepping the UPDATING file for the port name. It is too conservative: some of the updating entries are years old. To improve this, UPDATING needs to be parsed to extract individual entries and up should flag a port as manual only if the entry that mentions it is newer than the installed version of the port.

## Fetching port tree
It is disabled in 0.0.1 branch, but enabled on master. It is not designed to handle first-time runs of portsnap, but does definitely work for the situations when the user already ran portsnap once. Should that be a concern at all?

