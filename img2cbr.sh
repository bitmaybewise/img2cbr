#!/bin/bash

F=$1

find $F -type d -mindepth 2 \
    -exec zip -r "{}.cbr" "{}" \;
