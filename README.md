# img2cbr

Convert a folder containing images to a cbr file, keeping the folder structure.

# Usage

Usage of img2cbr:

    $ img2cbr -h
    Usage of img2cbr:
        -d int
                directory depth (default 1)
        -i string
                directory of origin
        -o string
                directory of destination
        -v	verbose output

It will look for every folder given a depth, and output the converted files to the destination folder, keeping the same folder structure.

    $ img2cbr -i /my/folder/of/origin -o /my/folder/of/destination -d 2 -v

Destination folder will be equal to origin by default, if not specified.

    $ img2cbr -i /my/folder/of/origin
