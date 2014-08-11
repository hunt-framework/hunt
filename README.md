Hunt [![Build Status](https://travis-ci.org/hunt-framework/hunt.svg)](https://travis-ci.org/hunt-framework/hunt)
====================

Hunt is a flexible, lightweight search platform written in Haskell.

The default server implementation provides a powerful JSON-API and a simple web interface.

Features
----

- Powerful query language
- Schema support (numeric data, dates, geospatial data)
- Granular ranking capabilities
- JSON API
- Extensible architecture


Installation
----

##### Dependencies

  - [GHC](https://www.haskell.org/ghc/): The Glasgow Haskell Compiler
  - [Cabal](http://www.haskell.org/cabal/): Haskell package management tool

<!--- - Compression
  - [zlib][zlib] compression library
  - [bzip2][bzip] compression library
  - [Snappy][snappy] compression library
deb: `apt-get install ghc cabal-install zlib1g-dev libbz2-dev libsnappy-dev`
-->

##### Hunt Installation

The easiest way to get the setup started is to install the current [Haskell Platform](https://www.haskell.org/platform/). 

##### Linux
----

If you're using linux, you are able to use our Makefile for the build.

```bash
git clone https://github.com/hunt-framework/hunt.git
cd hunt
make sandbox install
```

##### Windows
----

If you're using Windows, you can use cabal for the build. If you like to use sandboxes on Windows, you can copy the neccessary cabal commands from our Makefile. 

```bash
git clone https://github.com/hunt-framework/hunt.git
cd hunt/hunt-server
cabal install
```

Getting Started
----

The following line starts the default server.
The web interface is available at [http://localhost:3000/](http://localhost:3000/).

```bash
make startServer
```

A small sample data set can be inserted with:

```bash
make insertJokes
```


FAQ
----
<!---
##### The installation fails with "Missing dependency on a foreign library: * Missing (or bad) header file: zlib.h * Missing C library: z".
zlib1g-dev
The [zlib][zlib] compression library is missing. Install from source or use the distribution
packages (deb: `zlib1g-dev`).

##### The installation fails with "Missing dependency on a foreign library: * Missing (or bad) header file: bzlib.h * Missing C library: bz2".
The [bzip2][bzip] compression library is missing. Install from source or use the distribution
packages (deb: `libbz2-dev`).

##### The installation fails with "...Snappy failure message...".
The [Snappy][snappy] compression library is missing. Install from source or use the distribution
packages (deb: `libsnappy-dev`).
-->

##### Can i run Hunt on a 32Bit machine.
This would not be a good idea. First of all we are using 64Bit Hashes for our internal `DocId`s. Despite this source of error the 32Bit machine limits the available memory to 4GB. 

##### Why is the CPU usage in idle so high?
GHC performs a a major garbage collection every 0.3 seconds in idle, which can be computationally
expensive on a big index. This can be disabled with the [GHC RTS option][ghc-rts] `-I0`.


Development / History
----

Hunt was started in 2013 by [Ulf Sauer][ulf] and [Chris Reumann][chris] to improve and extend the
existing [Holumbus][holumbus] framework.
Holumbus was developed in 2008-2009 by [Timo B. Kranz][timo] and [Sebastian M. Gauck][sebastian] and
powers the current Haskell API search [Hayoo!][hayoo].
We decided to rebrand, because Hunt represents a major rewrite and breaks compatibility.

A new Hayoo implementation is currently under development by [Sebastian Philipp][seb].

Both projects were developed at the [FH Wedel][fhwedel] under supervision and active support of
[Prof. Dr. Uwe Schmidt][uwe].



[hunt-tutorial]: https://github.com/hunt-framework/hunt/tba "Hunt Tutorial"
[hunt-wiki]:     https://github.com/hunt-framework/hunt/tba "Hunt Wiki"

[fhwedel]:       http://www.fh-wedel.de/                    "FH-Wedel: University of Apllied Sciences"
[holumbus]:      https://github.com/fortytools/holumbus     "Holumbus Framework"
[hayoo]:         http://holumbus.fh-wedel.de/hayoo          "Hayoo"

[seb]:           https://github.com/sebastian-philipp       "GitHub: Sebastian Philipp"
[chris]:         https://github.com/chrisreu                "GitHub: Chris Reumann"
[ulf]:           https://github.com/ulfs                    "GitHub: Ulf Sauer"
[timo]:          https://twitter.com/tbk303                 "Twitter: Timo B. Kranz"
[sebastian]:     https://twitter.com/sgauck                 "Twitter: Sebastian M. Gauck"
[uwe]:           http://www.fh-wedel.de/~si/                "FH-Wedel: Prof. Dr. Uwe Schmidt"

[ghc-rts]:       https://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html "GHC RTS options"

[zlib]:          http://www.zlib.net/                       "zlib"
[bzip]:          http://bzip.org/                           "bzip2"
[snappy]:        https://code.google.com/p/snappy/          "Snappy"
