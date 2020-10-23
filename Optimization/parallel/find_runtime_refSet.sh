#/bin/bash
MODEL=LakeComo
NSEEDS=10
NSTEPS=99
SEEDS=$(seq 1 ${NSEEDS})
STEPS=$(seq 0 ${NSTEPS})

for SEED in ${SEEDS}
do
	for STEP in ${STEPS}
	do
		python pareto.py ./objs/runtime/${MODEL}_S${SEED}_M*/*_step${STEP}.obj -o 0-2 -e 0.5 25.0 0.05 \
		--output ./objs/runtime/reference/${MODEL}_S${SEED}_step${STEP}.runref --delimiter=" " --comment="#" --blank
		python pareto.py ./runtime/${MODEL}_S${SEED}_M*/*_step${STEP}.set -o 134-136 -e 0.5 25.0 0.05 \
		--output ./runtime/reference/${MODEL}_S${SEED}_step${STEP}.runset --delimiter=" " -c "#" "//" --blank
	done
done