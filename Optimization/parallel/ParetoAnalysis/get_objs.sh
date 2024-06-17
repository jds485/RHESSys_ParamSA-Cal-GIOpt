#!/bin/bash
MODEL=GInolic
SEED=7
NISLANDS=1
ISLANDS=$(seq 0 ${NISLANDS})

for ISLAND in ${ISLANDS}
do
awk 'BEGIN {FS=" "}; /^#/ {print $0}; /^[^#/]/ {printf("%s %s %s\n",$7,$8,$9)}' ./runtime/${MODEL}_S${SEED}_M${ISLAND}.runtime \
>./objs/runtime/${MODEL}_S${SEED}_M${ISLAND}.obj
done