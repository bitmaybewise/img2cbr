# img2cbr

Convert a folder containing images to a cbr file, keeping the folder structure.

# Usage

Usage of img2cbr:

    $ img2cbr -h
    img2cbr - converts a folder containing images to a cbr file

    Usage: img2cbr (-i|--origin ARG) (-o|--destination ARG) [-d|--depth ARG]
                   [-p|--pool ARG] [-v|--verbose]
    Available options:
    -i,--origin ARG          directory of origin
    -o,--destination ARG     directory of destination
    -d,--depth ARG           directory depth
    -p,--pool ARG            number of parallel convertions
    -v,--verbose             verbose output
    -h,--help                Show this help text

It will look for every folder given a depth, and output the converted files to the destination folder, keeping the same folder structure.

    $ img2cbr -i /my/folder/of/origin -o /my/folder/of/destination -d 2 -v

Destination folder will be equal to origin by default, if not specified.

    $ img2cbr -i /my/folder/of/origin

# Development

Requirements: 
1. `GHC`
1. `cabal`

Versions used to compile:

    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 9.6.3

    $ cabal --version
    cabal-install version 3.10.1.0
    compiled using version 3.10.1.0 of the Cabal library

Compiling:

    $ make
    cabal build
    ...
    Resolving dependencies...
    Symlinking 'img2cbr' to 'bin/img2cbr'
    mv bin/img2cbr bin/img2cbr-$PLATFORM

PS: not tested on previous versions of GHC.

