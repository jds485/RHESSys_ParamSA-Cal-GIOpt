#!/bin/bash
MODEL=GInolic
SEED=7
JAVA_ARGS="-cp MOEAFramework-2.13-Demo.jar"

java ${JAVA_ARGS} org.moeaframework.analysis.sensitivity.ResultFileEvaluator \
-d 3 -i ./objs/runtime/reference/${MODEL}_S${SEED}.runref -r ${MODEL}.reference \
-o ./metrics/runtime/${MODEL}_S${SEED}.metrics
