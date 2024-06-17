#/bin/bash
MODEL=GInolic
SEED=8
NSTEPS=378
STEPS=$(seq 0 ${NSTEPS})

for STEP in ${STEPS}
do
	python pareto.py ./objs/runtime/${MODEL}_S${SEED}_M*/*_step${STEP}.obj -o 0-2 -e 0.015 0.015 1.0 \
	--output ./objs/runtime/reference/${MODEL}_S${SEED}_step${STEP}.runref --delimiter=" " --comment="#" --blank
	python pareto.py ./runtime/${MODEL}_S${SEED}_M*/*_step${STEP}.set -o 6-8 -e 0.015 0.015 1.0 \
	--output ./runtime/reference/${MODEL}_S${SEED}_step${STEP}.runset --delimiter=" " -c "#" "//" --blank
done
