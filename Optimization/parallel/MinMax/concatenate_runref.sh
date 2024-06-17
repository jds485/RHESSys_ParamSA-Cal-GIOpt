#!/bin/bash
MODEL=GI
SEED=9
NSTEPS=45
STEPS=$(seq 0 ${NSTEPS})

for STEP in ${STEPS}
do
  echo "#" >> ./objs/runtime/reference/${MODEL}_S${SEED}_step${STEP}.runref 
  cat ./objs/runtime/reference/${MODEL}_S${SEED}_step${STEP}.runref 
  cat ./objs/runtime/reference/${MODEL}_S${SEED}_step${STEP}.runref >> ./objs/runtime/reference/${MODEL}_S${SEED}.runref
  echo "#" >> ./runtime/reference/${MODEL}_S${SEED}_step${STEP}.runset 
  cat ./runtime/reference/${MODEL}_S${SEED}_step${STEP}.runset
  cat ./runtime/reference/${MODEL}_S${SEED}_step${STEP}.runset >> ./runtime/reference/${MODEL}_S${SEED}.runset
done
