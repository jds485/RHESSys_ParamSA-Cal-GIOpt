#!/bin/bash
MODEL=LakeComo
NSEEDS=10
SEEDS=$(seq 1 ${NSEEDS})
JAVA_ARGS="-cp MOEAFramework-2.13-Demo.jar"

for SEED in ${SEEDS}
do
    java ${JAVA_ARGS} org.moeaframework.analysis.sensitivity.ResultFileEvaluator \
    -d 3 -i ./sets/${MODEL}_S${SEED}.set -r ${MODEL}.reference -o ./metrics/${MODEL}_S${SEED}.metrics
done
