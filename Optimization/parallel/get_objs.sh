#!/bin/bash
MODEL=LakeComo
NSEEDS=10
NISLANDS=1
SEEDS=$(seq 1 ${NSEEDS})
ISLANDS=$(seq 0 ${NISLANDS})

for SEED in ${SEEDS}
do
	for ISLAND in ${ISLANDS}
	do
	awk 'BEGIN {FS=" "}; /^#/ {print $0}; /^[^#/]/ {printf("%s %s %s\n",$135,$136,$137)}' ./runtime/${MODEL}_S${SEED}_M${ISLAND}.runtime \
	>./objs/runtime/${MODEL}_S${SEED}_M${ISLAND}.obj
	done
done