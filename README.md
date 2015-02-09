up
==

Software upgrader for FreeBSD, a frontend for portsnap,
portmaster/portupgrade/makefile. Tries to be smart by only upgrading software
that doesn't have any mentions in UPDATING.

# TODO in future versions
- Enable interaction with portsnap (updating ports tree)

# Prerequisites
- ghc
- cabal

# Building
- cabal configure
- cabal build

Configure is only necessary at the first build.
