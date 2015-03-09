Tangles
=======

Tangles is a Haskell library implementing some enumeration algorithms for knot theory objects,
such as k-tangle projections, alternating k-tangles, non-alternating k-tangles, alternating
virtual links. Also it can compute Jones and Kauffman F polynomial invariants of links and
tangles and create nice drawings of tangles automatically.

See following papers for further details:
  - http://arxiv.org/abs/0712.3859
  - http://www.worldscientific.com/doi/abs/10.1142/S0218216512500691?journalCode=jktr
  - a few in tangles-tex repository (unfortunatelly most are in russian yet)


Building
--------

To build Tangles you will need:

  - GCC with g++ at least 4.7
  - GHC at least 7.6
  - Cabal at least 1.14
  - GNU Scientific Library (GSL)
  - a few packages from http://hackage.haskell.org/

Building steps:

    $ cabal configure
    $ cabal build

Cabal will notify you what packages are requires and they can be installed with "cabal install" or
(preferred way if possible) with package manager of your Linux distribution. Under Windows you can
for example build GSL by youself with MinGW and integrate its binaries into the copy of MinGW inside
Haskell Platform.


Building tests
--------------

Package contains a few test modules that can be enabled by switching enable-test-modules flag. After
that standard cabal testing support can be used.

    $ cabal configure --enable-tests -fenable-test-modules

It is rather ugly ad hoc solution, but it is neccessary due to test modules using some internal modules
that are not exported from package. And I have not found better way yet.

Also there are a few benckmarks there that can be enabled by separate flags. For details see
tangles.cabal file.
