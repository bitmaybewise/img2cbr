#!/bin/bash

function destiny_dir()
{
    local ORIGIN_DIR="$1"
    local DESTINY_DIR=${ORIGIN_DIR/original/cbr}
    echo "$DESTINY_DIR"
}

function mkdir_destiny()
{
    DIR=$(destiny_dir "$1")
    mkdir -p "$DIR"
}

function convert2cbr()
{
    local ORIGIN_DIR="$1"
    local DESTINY_DIR=$(destiny_dir "$ORIGIN_DIR")
    local CBR_FILE="$DESTINY_DIR.cbr"
    zip -r "$CBR_FILE" "$ORIGIN_DIR"
}
export -f destiny_dir
export -f mkdir_destiny
export -f convert2cbr

DEPTH=2
DEPTH_OUTPUT_FOLDER=1
ROOT_FOLDER=$1

# create output folder
find $ROOT_FOLDER -type d -mindepth $DEPTH_OUTPUT_FOLDER -maxdepth $DEPTH_OUTPUT_FOLDER \
    -exec bash -c 'mkdir_destiny "{}"' bash {} \;

# convert files
find $ROOT_FOLDER -type d -mindepth $DEPTH -maxdepth $DEPTH \
    -exec bash -c 'convert2cbr "{}"' bash {} \;
