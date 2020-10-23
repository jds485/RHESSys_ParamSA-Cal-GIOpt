#!/bin/bash
MODEL=LakeComo
NSEEDS=10
SEEDS=$(seq 1 ${NSEEDS})
JAVA_ARGS="-cp MOEAFramework-2.13-Demo.jar"

for SEED in ${SEEDS}
do
	java ${JAVA_ARGS} org.moeaframework.analysis.sensitivity.ResultFileEvaluator \
    -d 3 -i ./objs/runtime/reference/${MODEL}_S${SEED}.runref -r ${MODEL}.reference \
    -o ./metrics/runtime/${MODEL}_S${SEED}.metrics
done
