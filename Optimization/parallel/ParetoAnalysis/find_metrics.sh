#!/bin/bash
MODEL=GInolic
SEED=7
JAVA_ARGS="-cp MOEAFramework-2.13-Demo.jar"

java ${JAVA_ARGS} org.moeaframework.analysis.sensitivity.ResultFileEvaluator \
-d 3 -i ./sets/${MODEL}_S${SEED}.set -r ${MODEL}.reference -o ./metrics/${MODEL}_S${SEED}.metrics
